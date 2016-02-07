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

module Reactive.DOM.XHR (

      xhr
    , xhrImmediate'
    , xhrImmediate

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
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
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

xhr :: Banana.Event XHRRequest -> Compose MomentIO Banana.Event XHRResponse
xhr requests = Compose $ do
    let spawns :: Banana.Event (MomentIO (Banana.Event XHRResponse, XMLHttpRequest))
        spawns = makeXHRFromRequest <$> requests
    commuted <- execute spawns
    let events = switchE (fst <$> commuted)
    inFlight <- stepper Nothing (Just . snd <$> commuted)
    let cancelInFlight :: Maybe XMLHttpRequest -> IO ()
        cancelInFlight in_xhr = case in_xhr of
            Nothing -> pure ()
            Just in_flight -> do
                readyState <- getReadyState in_flight
                if readyState /= 4
                then XHR.abort in_flight
                else pure ()
    -- Every time commuted fires, we check the in-flight XHR and cancel it if
    -- it's not in ready state 4. Careful to choose commuted rather than
    -- events, as the latter is the actual response from an XHR.
    reactimate (cancelInFlight <$> (inFlight <@ commuted))
    pure events

-- | Like XHR but send it off immediately. We use Identity for uniformity:
--   both xhrImmediate' and xhr take the XHRRequest inside some functor.
xhrImmediate' :: Identity XHRRequest -> MomentIO (Banana.Event XHRResponse, XMLHttpRequest)
xhrImmediate' request = makeXHRFromRequest (runIdentity request)

xhrImmediate :: Identity XHRRequest -> Compose MomentIO Banana.Event XHRResponse
xhrImmediate = Compose . fmap fst . xhrImmediate'

-- | Make and send an XHR. You get the event giving the response, and the
--   XHR itself, in case perhaps you want to cancel it.
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
    open xhrObject (show (xhrRequestMethod xhrRequest))
                   (reqUrl)
                   (Just True) -- True meaning do not block
                   (username)
                   (password)
    forM_ reqHeaders (uncurry (setRequestHeader xhrObject))
    thread <- case xhrRequestBody xhrRequest of
        Nothing -> liftIO (async (send xhrObject))
        Just txt -> liftIO (async (sendString xhrObject (lazyTextToJSString txt)))
    (ev, fire) <- newEvent
    -- When the state changes to 4, we build an XHRResponse and fire the event.
    liftIO $ on xhrObject readyStateChange $ do
                 readyState <- getReadyState xhrObject
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
