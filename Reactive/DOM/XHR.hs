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
{-# LANGUAGE Arrows #-}

module Reactive.DOM.XHR (

      XHRHandler
    , xhrHandler

    {-
    , XHRFolder
    , xhrFolder
    , xhrFolderTrivial
    -}

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

import Prelude hiding ((.), id)
import Control.Category
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

-- Want to be able to make a kind of XHR handler fold, in which subsequent
-- requests may use data returned from prior responses.
-- Well, if we deal with XHRHandler s s then 

-- | Given an input s, make an XHRRequest and a function to handle the
--   XHRResponse. It also allows pure functions, making it a Category and an
--   Arrow.
newtype XHRHandler s t = XHRHandler {
      runXHRHandler :: s -> Either t (XHRRequest, XHRHandler XHRResponse t)
    }

instance Profunctor XHRHandler where
    dimap l r (XHRHandler f) = XHRHandler $ \s -> case f (l s) of
        Left x -> Left (r x)
        Right (req, res) -> Right (req, rmap r res)

-- | Category composition of an XHRHandlers means do the left after the
--   right.
instance Category XHRHandler where
    id = XHRHandler Left
    left . right = XHRHandler $ \s -> case runXHRHandler right s of
        Left t -> runXHRHandler left t
        Right (req, res) -> Right (req, left . res)

instance Arrow XHRHandler where
    arr f = XHRHandler (Left . f)
    first (XHRHandler f) = XHRHandler $ \(s, c) -> case f s of
        Left t -> Left (t, c)
        Right (req, res) -> Right (req, arr (\x -> (x, c)) . res)

-- | Whereas Category composition will sequence requests, ArrowChoice
--   disjunction will choose between requests.
instance ArrowChoice XHRHandler where
    left (XHRHandler f) = XHRHandler $ \choice -> case choice of
        Left x -> case f x of
            Left y -> Left (Left y)
            Right (req, res) -> Right (req, arr Left . res)
        Right y -> Left (Right y)

instance ArrowApply XHRHandler where
    app = XHRHandler $ \(handler, input) -> runXHRHandler handler input

xhrHandler :: (s -> (XHRRequest, XHRResponse -> t)) -> XHRHandler s t
xhrHandler f = XHRHandler $ \s ->
    let (req, resToT) = f s
        res = XHRHandler $ \r -> Left (resToT r)
    in  Right (req, res)
    
newtype XhrContinuation t = XhrContinuation {
      getXhrContinuation :: Either t (XMLHttpRequest, Banana.Event (XhrContinuation t))
    }

runXhrContinuation
    :: forall t .
       XhrContinuation t
    -> Either t (XMLHttpRequest, (Banana.Event (Either XMLHttpRequest t)))
runXhrContinuation xhrcont = case getXhrContinuation xhrcont of
    Left t -> Left t
    Right (xhr, rest) -> 
        let stripped :: Banana.Event (Either t (XMLHttpRequest, Banana.Event (Either XMLHttpRequest t)))
            stripped = runXhrContinuation <$> rest
            -- immediate and delayed are mutually exclusive. If immediate fires,
            -- delayed is never.
            immediate :: Banana.Event t
            immediate = filterJust (either Just (const Nothing) <$> stripped)
            delayed :: Banana.Event (XMLHttpRequest, Banana.Event (Either XMLHttpRequest t))
            delayed = filterJust (either (const Nothing) Just <$> stripped)
            switched :: Banana.Event (Either XMLHttpRequest t)
            switched = switchE (snd <$> delayed)
            firstXhr :: Banana.Event XMLHttpRequest
            firstXhr = fst <$> delayed
            unioned = unionWith const (Right <$> immediate)
                      (unionWith const (Left <$> firstXhr) switched)
        in  Right (xhr, unioned)

evalXhrHandler
    :: forall s t .
       XHRHandler s t
    -> s
    -> MomentIO (XhrContinuation t)
evalXhrHandler (XHRHandler f) s = case f s of
    Left t -> pure . XhrContinuation . Left $ t
    Right (req, res) -> do
        -- Spawn this request right away.
        (ev, xhr) :: (Banana.Event XHRResponse, XMLHttpRequest)
            <- makeXHRFromRequest req
        -- Whenever the response comes, run the response handler on it to
        -- yield a continuation.
        next :: Banana.Event (XhrContinuation t)
            <- execute (evalXhrHandler res <$> ev)
        pure . XhrContinuation . Right $ (xhr, next)

{-
-- | An XHRHandler which may depend upon the output type.
--   These are used by xhrLimited, which feeds the latest response back into
--   the folder so that subsequent requests may depend upon it.
data XHRFolder s t where
    XHRFolder :: XHRHandler r q -> (Maybe q -> s -> Maybe r) -> (q -> t) -> XHRFolder s t

instance Profunctor XHRFolder where
    dimap l r (XHRFolder h f g) = XHRFolder h f' g'
      where
        g' = fmap r g
        f' x s = f x (l s)

runXHRFolder :: XHRFolder s t -> Maybe t -> s -> Maybe (XHRHandler () t)
runXHRFolder (XHRFolder h f g) maybet s = case f maybet s of
    Nothing -> Nothing
    Just r -> Just (arr (const r) >>> h >>> arr g)

{-
runXHRFolder' :: XHRFolder s t -> XHRHandler (Maybe t, s) (Maybe t)
runXHRFolder' (XHRFolder h f) = proc (maybet, s) -> do
    case f maybet s of
        Nothing -> do returnA -< Nothing
        Just r -> do
            t <- h -< r
            returnA -< Just t
-}

xhrFolder :: (Maybe t -> s -> Maybe r) -> XHRHandler r t -> XHRFolder s t
xhrFolder f h = XHRFolder h f id

xhrFolderTrivial :: XHRHandler s t -> XHRFolder s t
xhrFolderTrivial h = XHRFolder h (const Just) id
-}

-- | Run an XHRHandler immediately. 
xhr1 :: XHRHandler s t -> s -> MomentIO (Eventually t)
xhr1 handler s = do
    cont <- evalXhrHandler handler s
    case runXhrContinuation cont of
        Left t -> pure (immediately t)
        Right (_, ev) -> pure (delayed (filterJust (either (const Nothing) Just <$> ev)))

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

    continuations :: Banana.Event (Either t (XMLHttpRequest, (Banana.Event (Either XMLHttpRequest t))))
        <- execute ((fmap . fmap) runXhrContinuation (evalXhrHandlerAndCancel handler <$> currentXhr <@> sev))

    let immediates :: Banana.Event t
        immediates = filterJust (either Just (const Nothing) <$> continuations)

    let delayeds :: Banana.Event (Either XMLHttpRequest t)
        delayeds = switchE (snd <$> (filterJust (either (const Nothing) Just <$> continuations)))

    let firstXhr = fst <$> (filterJust (either (const Nothing) Just <$> continuations))
    let delayedValues = filterJust (either (const Nothing) Just <$> delayeds)
    let delayedXhrs = filterJust (either Just (const Nothing) <$> delayeds)

    -- Xhr changes to a Just whenever a new one appears from delayeds, and
    -- to Nothing whenever we actually hear a response (I assume, perhaps
    -- incorrectly, that if we abort() an Xhr, its readystate callback will
    -- not fire).
    let setXhr :: Banana.Event (Maybe XMLHttpRequest)
        setXhr = Just <$> (unionWith const delayedXhrs firstXhr)
    let unsetXhr :: Banana.Event (Maybe XMLHttpRequest)
        unsetXhr = const Nothing <$> delayedValues
    let changeXhr = unionWith const setXhr unsetXhr
    reactimate (const (Prelude.print "set") <$> setXhr)
    reactimate (const (Prelude.print "unset") <$> unsetXhr)
    currentXhr :: Behavior (Maybe XMLHttpRequest)
        <- stepper Nothing changeXhr

    pure (unionWith const immediates delayedValues)

  where

    evalXhrHandlerAndCancel
        :: XHRHandler s t
        -> Maybe XMLHttpRequest
        -> s
        -> MomentIO (XhrContinuation t)
    evalXhrHandlerAndCancel handler maybeXhr s = do
        cancelInFlight maybeXhr
        evalXhrHandler handler s

    cancelInFlight :: Maybe XMLHttpRequest -> MomentIO ()
    cancelInFlight in_xhr = case in_xhr of
        Nothing -> do
            liftIO (putStrLn "cancelInFlight : Nothing")
            pure ()
        Just in_flight -> do
            liftIO (putStrLn "cancelInFlight : Just")
            liftIO (XHR.abort in_flight)
            --readyState <- liftIO (getReadyState in_flight)
            --if readyState /= 4
            --then liftIO (XHR.abort in_flight)
            --else pure ()


{-
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

applyCurrentResponder
    :: forall t .
       Maybe (XHRResponse -> t)
    -> XHRResponse
    -> Maybe t
applyCurrentResponder Nothing _ = trace "Got response but have no responder. This is a bug." Nothing
applyCurrentResponder (Just f) res = Just (f res)
-}

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
    :: forall r s t .
       ( Monoid r )
    => XHRHandler (r, s) (r, Maybe t)
    -> Banana.Event s
    -> Compose MomentIO Banana.Event t
xhrLimited handler sev = Compose $ mdo

    -- TBD may be useful to log whenever a Nothing appears?

    let runIt :: r -> s -> XHRHandler () (r, Maybe t)
        runIt r s = lmap (const (r, s)) handler
    let handlers :: Banana.Event (XHRHandler () (r, Maybe t))
        handlers = runIt <$> mostRecentEvaluatedResponse <@> sev

    continuations :: Banana.Event (Either (r, Maybe t) (XMLHttpRequest, Banana.Event (Either XMLHttpRequest (r, Maybe t))))
        <- execute ((fmap . fmap) runXhrContinuation (flip evalXhrHandler () <$> willSpawn))

    -- We identify the handlers which fire while there's no in-flight xhr,
    -- and those which fire when there is an in-flight xhr.
    let passed :: Banana.Event (XHRHandler () (r, Maybe t))
        passed = whenE pass handlers
    let held :: Banana.Event (XHRHandler () (r, Maybe t))
        held = whenE hold handlers
    -- Held handlers must be enqueued so that we can run them when the in-flight
    -- returns.
    let changeWaiting :: Banana.Event (Maybe (XHRHandler () (r, Maybe t)))
        changeWaiting = unionWith const (Just <$> held) (const Nothing <$> delayedValues)
    waiting :: Behavior (Maybe (XHRHandler () (r, Maybe t)))
        <- stepper Nothing changeWaiting

    -- Fire the queued thing whenever delayed values comes back (signals the
    -- end of whatever xhr is out there).
    let queued :: Banana.Event (XHRHandler () (r, Maybe t))
        queued = filterJust (waiting <@ delayedValues)

    let willSpawn :: Banana.Event (XHRHandler () (r, Maybe t))
        willSpawn = unionWith const passed queued

    let immediates :: Banana.Event (r, Maybe t)
        immediates = filterJust (either Just (const Nothing) <$> continuations)

    let delayeds :: Banana.Event (Either XMLHttpRequest (r, Maybe t))
        delayeds = switchE (snd <$> (filterJust (either (const Nothing) Just <$> continuations)))

    -- delayedValues indicates not only the value, but also the end of an xhr,
    -- so it's an important control mechanism.
    let firstXhr = fst <$> (filterJust (either (const Nothing) Just <$> continuations))
    let delayedValues = filterJust (either (const Nothing) Just <$> delayeds)
    let delayedXhrs = filterJust (either Just (const Nothing) <$> delayeds)

    -- Xhr changes to a Just whenever a new one appears from delayeds, and
    -- to Nothing whenever we actually hear a response (I assume, perhaps
    -- incorrectly, that if we abort() an Xhr, its readystate callback will
    -- not fire).
    let setXhr :: Banana.Event (Maybe XMLHttpRequest)
        setXhr = Just <$> (unionWith const delayedXhrs firstXhr)
    let unsetXhr :: Banana.Event (Maybe XMLHttpRequest)
        unsetXhr = const Nothing <$> delayedValues
    let changeXhr = unionWith const setXhr unsetXhr
    currentXhr :: Behavior (Maybe XMLHttpRequest)
        <- stepper Nothing changeXhr

    let hold :: Behavior Bool
        hold = isJust <$> currentXhr
    let pass :: Behavior Bool
        pass = isNothing <$> currentXhr

    mostRecentEvaluatedResponse :: Behavior r
        <- stepper mempty (fst <$> delayedValues)

    pure (filterJust (snd <$> unionWith const immediates delayedValues))

{-

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
    let responses :: Banana.Event XHRResponse
        responses = switchE (fst <$> outs)
    let evaluatedResponses :: Banana.Event (Maybe t)
        evaluatedResponses = applyCurrentResponder <$> currentResponder <@> responses

    -- We track the last evaluated response so that we can feed it back into
    -- the next request. 
    mostRecentEvaluatedResponse :: Behavior (Maybe t)
        <- stepper Nothing evaluatedResponses

    pure (filterJust evaluatedResponses)
-}

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
