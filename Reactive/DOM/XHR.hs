{-|
Module      : Reactive.DOM.XHR
Description : Definition of reactive XHR-related things.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecursiveDo #-}

module Reactive.DOM.XHR (

      XHRHandler
    , xhrHandler
    , xhrHandlerSum
    , xhr1
    , xhrMany
    , xhrLimited

    , XHRURL
    , XHRStatus
    , XHRHeaders
    , XHRMethod(..)
    , XHRRequest(..)
    , XHRResponse(..)

    ) where

import Control.Arrow
import Control.Monad (forM_, when)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Data.Functor.Identity
import Data.Functor.Compose
import Data.IORef
import Data.Unique
import Data.Monoid
import Data.Profunctor
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Maybe (isNothing, isJust)
import Data.JSString.Text
import GHCJS.Types
import GHCJS.DOM.Types
import GHCJS.DOM.Element as Element
import GHCJS.DOM.XMLHttpRequest as XHR
import GHCJS.DOM.JSFFI.XMLHttpRequest as XHR hiding (send, cancel)
import GHCJS.DOM.EventM
import Reactive.Banana.Combinators as Banana
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Debug.Trace

type XHRURL = T.Text

type XHRStatus = Int

type XHRHeaders = [(T.Text, T.Text)]

data XHRMethod = GET | PUT | POST | DELETE

instance Show XHRMethod where
    show method = case method of
        GET -> "GET"
        PUT -> "PUT"
        POST -> "POST"
        DELETE -> "DELETE"

data XHRRequest = XHRRequest {
      xhrRequestMethod :: XHRMethod
    , xhrRequestURL :: XHRURL
    , xhrRequestHeaders :: XHRHeaders
    , xhrRequestBody :: Maybe TL.Text
    , xhrRequestAuth :: Maybe (T.Text, T.Text)
    }

deriving instance Show XHRRequest

data XHRResponse = XHRResponse {
      xhrResponseStatus :: XHRStatus
    , xhrResponseHeaders :: XHRHeaders
    , xhrResponseBody :: Maybe TL.Text
    }

deriving instance Show XHRResponse

newtype XHRHandler s t = XHRHandler {
      runXHRHandler :: s -> (XHRRequest, XHRResponse -> t)
    }

instance Profunctor XHRHandler where
    dimap l r (XHRHandler f) = XHRHandler $ \s ->
        let (req, mkRes) = f (l s)
        in  (req, fmap r mkRes)

xhrHandler :: (s -> (XHRRequest, XHRResponse -> t)) -> XHRHandler s t
xhrHandler = XHRHandler

-- | Now suppose we have two XHRHandlers and we want to combine them?
--        XHRHandler sL tL
--     -> XHRHandler sR tR
--     -> XHRHandler (Either sL sR) (Either tL tR)
--   This is like +++ from Arrow, but of course XHRHandler is not even
--   a category.
--   So maybe there's no typeclass for this, but here it is anyway.
xhrHandlerSum
    :: XHRHandler sL tL
    -> XHRHandler sR tR
    -> XHRHandler (Either sL sR) (Either tL tR)
xhrHandlerSum (XHRHandler l) (XHRHandler r) = XHRHandler $ \choice -> case choice of
    Left sL -> (fmap . fmap) Left (l sL)
    Right sR -> (fmap . fmap) Right (r sR)

xhr1 :: XHRHandler s t -> s -> Compose MomentIO Banana.Event t
xhr1 handler s = Compose $ do
    let (request, mkResponse) = runXHRHandler handler s
    (ev, _) <- makeXHRFromRequest request
    pure (mkResponse <$> ev)

-- | For every occurrence of an event, use an XHRHandler to launch a request
--   and handle it. If the event fires before a response is heard, the response
--   will be discarded and the response of the next XHR will be taken.
--   Since aborting an XHR does not guarantee a lack of remote side-effects
--   (aborting a POST doesn't mean the POST won't have effect, for example)
--   this is not appropriate for side-effecting XHRs. See xhrLimited instead.
xhrMany
    :: forall s t .
       XHRHandler s t
    -> Banana.Event s
    -> Compose MomentIO Banana.Event t
xhrMany handler sev = Compose $ mdo
    let requestsAndResponders = runXHRHandler handler <$> sev
    let requests :: Banana.Event XHRRequest
        requests = fst <$> requestsAndResponders
    let responders :: Banana.Event (XHRResponse -> t)
        responders = snd <$> requestsAndResponders
    let spawns :: Banana.Event (MomentIO (Banana.Event XHRResponse, XMLHttpRequest))
        spawns = spawnRequest <$> inFlight <@> requests
    -- Track the current responder. Goes to Just whenever a request is made,
    -- and then returns to Nothing whenever any response is retrieved.
    let changeResponder :: Banana.Event (Maybe (XHRResponse -> t))
        changeResponder = unionWith const
                                    (Just <$> responders)
                                    (const Nothing <$> responses)
    currentResponder :: Behavior (Maybe (XHRResponse -> t))
        <- stepper Nothing changeResponder
    outs :: Banana.Event (Banana.Event XHRResponse, XMLHttpRequest)
        <- execute spawns
    let changeInFlight :: Banana.Event (Maybe XMLHttpRequest)
        changeInFlight = unionWith const
                                   (Just . snd <$> outs)
                                   (const Nothing <$> responses)
    inFlight <- stepper Nothing changeInFlight
    let responses = switchE (fst <$> outs)
    pure (filterJust (applyCurrentResponder <$> currentResponder <@> responses))
  where
    -- Make a new request, canceling any in-flight request.
    spawnRequest
        :: Maybe XMLHttpRequest
        -> XHRRequest
        -> MomentIO (Banana.Event XHRResponse, XMLHttpRequest)
    spawnRequest inflight req = do
        liftIO (cancelInFlight inflight)
        makeXHRFromRequest req
    cancelInFlight :: Maybe XMLHttpRequest -> IO ()
    cancelInFlight in_xhr = case in_xhr of
        Nothing -> pure ()
        Just in_flight -> do
            readyState <- getReadyState in_flight
            if readyState /= 4
            then XHR.abort in_flight
            else pure ()

applyCurrentResponder
    :: forall t .
       Maybe (XHRResponse -> t)
    -> XHRResponse
    -> Maybe t
applyCurrentResponder Nothing _ = trace "Got response but have no responder. This is a bug." Nothing
applyCurrentResponder (Just f) res = Just (f res)

-- | Spawn an XHR for every XHRRequest in the event which occurrs when there is
--   no other XHR spawned as a result of that event. If the event fires multiple
--   times while there's one in-flight, then the last occurrence will be used
--   to spawn an XHR as soon as the in-flight ends. No in-flight requests will
--   be aborted.
--
--   Compared to xhr, this one is more appropriate for effectful requests
--   (PUT/POST rather than GET) because aborting such a request does not
--   guarantee that the effects won't be realized.
--
xhrLimited
    :: forall s t .
       XHRHandler s t
    -> Banana.Event s
    -> Compose MomentIO Banana.Event t
xhrLimited handler sev = Compose $ mdo

    let requestsAndResponders :: Banana.Event (XHRRequest, XHRResponse -> t)
        requestsAndResponders = runXHRHandler handler <$> sev

    -- We have at most one queued request (input event fires while a request
    -- was in flight). Whenever we spawn a request we set it to Nothing.
    let changeQueuedRequest :: Banana.Event (Maybe (XHRRequest, XHRResponse -> t))
        changeQueuedRequest = unionWith const
                                        (const Nothing <$> spawns)
                                        (whenE inFlight (Just <$> requestsAndResponders))
    queuedRequest :: Behavior (Maybe (XHRRequest, XHRResponse -> t))
        <- stepper Nothing changeQueuedRequest

    -- In addition to the queued request, we also have the current in-flight
    -- request. It goes to Nothing whenever we get a response, and Just whenever
    -- we spawn one.
    let changeResponder :: Banana.Event (Maybe (XHRResponse -> t))
        changeResponder = unionWith const
                                    (Just . snd <$> spawnOne)
                                    (const Nothing <$> responses)
    currentResponder :: Behavior (Maybe (XHRResponse -> t))
        <- stepper Nothing changeResponder

    -- Existence or non-existence of an in-flight request is identified by the
    -- presence of a response handler.
    let noInFlight :: Behavior Bool
        noInFlight = isNothing <$> currentResponder
    let inFlight :: Behavior Bool
        inFlight = isJust <$> currentResponder

    -- We spawn whenever the input event fires and there's no in-flight, or
    -- whenever we get a response and there's a queued request.
    let spawnOne :: Banana.Event (XHRRequest, XHRResponse -> t)
        spawnOne = unionWith const
                             (whenE noInFlight requestsAndResponders)
                             (filterJust (queuedRequest <@ responses))
    let spawns :: Banana.Event (MomentIO (Banana.Event XHRResponse, XMLHttpRequest))
        spawns = makeXHRFromRequest . fst <$> spawnOne
    outs :: Banana.Event (Banana.Event XHRResponse, XMLHttpRequest)
        <- execute spawns
    let responses = switchE (fst <$> outs)
    pure (filterJust (applyCurrentResponder <$> currentResponder <@> responses))

-- | Make and send an XHR. You get the event giving the response, and the
--   XHR itself, in case perhaps you want to cancel it.
--
--   TODO There's something wrong in here w.r.t. undefined/error values in the
--   body. If we give one, the XHR dies, but the error doesn't propagate up to
--   the main thread, which can be rather confusing.
makeXHRFromRequest
    :: XHRRequest
    -> MomentIO (Banana.Event XHRResponse, XMLHttpRequest)
makeXHRFromRequest xhrRequest = do
    xhrObject <- newXMLHttpRequest
    let reqUrl = textToJSString (xhrRequestURL xhrRequest)
    let reqHeaders = (\(x, y) -> (textToJSString x, textToJSString y)) <$> (xhrRequestHeaders xhrRequest)
    let maybeAuth :: Maybe (T.Text, T.Text)
        maybeAuth = xhrRequestAuth xhrRequest
    let username :: Maybe JSString
        username = textToJSString . fst <$> maybeAuth
    let password :: Maybe JSString
        password = textToJSString . snd <$> maybeAuth
    --let traceString = mconcat [
    --          "makeXHRFromRequest "
    --        , show (xhrRequestMethod xhrRequest)
    --        , " to "
    --        , show (xhrRequestURL xhrRequest)
    --        , " with headers "
    --        , show (reqHeaders)
    --        ]
    --liftIO (putStrLn traceString)
    open xhrObject (show (xhrRequestMethod xhrRequest))
                   (reqUrl)
                   (Just True) -- True meaning do not block
                   (username)
                   (password)
    forM_ reqHeaders (uncurry (setRequestHeader xhrObject))
    thread <- case xhrRequestBody xhrRequest of
        Nothing -> liftIO $ do
            --putStrLn (mconcat ["makeXHRFromRequest empty body"])
            async (send xhrObject)
        Just txt -> liftIO $ do
            --putStrLn (mconcat ["makeXHRFromRequest body ", show txt])
            --putStrLn (mconcat ["makeXHRFromRequest body"])
            async $ do
                sendString xhrObject (lazyTextToJSString txt)
                --putStrLn (mconcat ["makeXHRFromRequest body sent"])
    (ev, fire) <- newEvent
    -- When the state changes to 4, we build an XHRResponse and fire the event.
    liftIO $ on xhrObject readyStateChange $ do
                 readyState <- getReadyState xhrObject
                 --let traceString = mconcat [
                 --          "makeXHRFromRequest changed ready state to "
                 --        , show readyState
                 --        ]
                 --liftIO (putStrLn traceString)
                 if readyState /= 4
                 then return ()
                 else do status <- getStatus xhrObject
                         -- getAllResponseHeaders just gives a string; we've got
                         -- to separate them into a list. MDN says they're
                         -- to be separated by CRLF.
                         Just responseHeaders <- getAllResponseHeaders xhrObject
                         let headers = marshallResponseHeaders responseHeaders
                         when (not (corsPreflight headers)) $ do
                             responseText <- getResponseText xhrObject
                             let response = XHRResponse (fromIntegral status)
                                                        (headers)
                                                        (lazyTextFromJSString <$> responseText)
                             liftIO $ fire response
    return (ev, xhrObject)

corsPreflight :: [(T.Text, T.Text)] -> Bool
corsPreflight = elem (T.toCaseFold "Access-Control-Allow-Origin") . fmap (T.toCaseFold . fst)

marshallResponseHeaders :: T.Text -> [(T.Text, T.Text)]
marshallResponseHeaders text =
    let brokenCRLF = T.splitOn "\r\n" text
        breakOnColon = \t -> let (key, value) = T.breakOn ":" t
                                 -- tail is partial, so we have to do this
                                 -- crude bool test.
                             in  if T.null value
                                 then (key, value)
                                 else (key, T.tail value)
    in  breakOnColon <$> brokenCRLF
