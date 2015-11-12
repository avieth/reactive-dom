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

module Reactive.DOM.XHR where

import Control.Monad (forM_)
import Control.Monad.Trans.Reader (ask)
import GHCJS.Types
import GHCJS.DOM.Types
import GHCJS.DOM.Element as Element
import GHCJS.DOM.XMLHttpRequest as XHR hiding (send)
import GHCJS.DOM.JSFFI.XMLHttpRequest as XHR
import GHCJS.DOM.EventM
import Reactive.Banana.Combinators as Banana
import Reactive.Banana.Frameworks

type XHRURL = JSString

type XHRStatus = Int

type XHRHeaders = [(JSString, JSString)]

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
    -- For now we just take a text body. Good enough for using JSON endpoints.
    , xhrRequestBody :: Maybe JSString
    }

-- You get only the status code and the response text.
data XHRResponse = XHRResponse {
      xhrResponseStatus :: XHRStatus
    , xhrResponseBody :: Maybe JSString
    }

-- It's weird that we have an event around the abort.
-- Hm, makes sense though. Each occurrence of the event corresponds to one
-- send of the XHR.
xhr :: Banana.Event XHRRequest -> MomentIO (Banana.Event (Banana.Event XHRResponse, IO ()))
xhr inputEvent = execute (makeXHRFromRequest <$> inputEvent)

makeXHRFromRequest :: XHRRequest -> MomentIO (Banana.Event XHRResponse, IO ())
makeXHRFromRequest xhrRequest = do
    xhrObject <- newXMLHttpRequest
    open xhrObject (show (xhrRequestMethod xhrRequest))
                   (xhrRequestURL xhrRequest)
                   (Just True)
                   (Nothing :: Maybe JSString)
                   (Nothing :: Maybe JSString)
    forM_ (xhrRequestHeaders xhrRequest) (uncurry (setRequestHeader xhrObject))
    (ev, fire) <- newEvent
    liftIO $ on xhrObject readyStateChange $ do
                 readyState <- getReadyState xhrObject
                 if readyState /= 4
                 then return ()
                 else do status <- getStatus xhrObject
                         responseText <- getResponseText xhrObject
                         liftIO $ fire (XHRResponse (fromIntegral status) responseText)
    case xhrRequestBody xhrRequest of
        Nothing -> send xhrObject
        Just str -> sendString xhrObject str
    return (ev, XHR.abort xhrObject)
