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

module Reactive.DOM.XHR where

import Control.Arrow
import Control.Monad (forM_)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Data.Functor.Identity
import Data.IORef
import Data.Unique
import Data.Monoid
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.EitherBoth
import qualified Data.Map as M
import GHCJS.Types
import GHCJS.DOM.Types
import GHCJS.DOM.Element as Element
import GHCJS.DOM.XMLHttpRequest as XHR
import GHCJS.DOM.JSFFI.XMLHttpRequest as XHR hiding (send)
import GHCJS.DOM.EventM
import Reactive.Banana.Combinators as Banana
import Reactive.Banana.Frameworks
import Reactive.Sequence

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
data XHRResponse body = XHRResponse {
      xhrResponseStatus :: XHRStatus
    , xhrResponseBody :: Maybe body
    }

-- | By judiciously choosing the type parameters @f@ and @g@, you can spawn
--   an XHR immediately (@f ~ Identity@).
xhr
    :: forall f g body .
       ( FromJSString body
       ,   SwitchesTo (Sequence f g (SEvent (XHRResponse body)))
         ~ SEvent (XHRResponse body)
         -- The above should always be true.
       , Switchable f (Const ()) g Identity
       , Unionable f (Const ()) g Identity
       )
    => (forall s . f (MomentIO s) -> MomentIO (f s))
    -> (forall s . g (MomentIO s) -> MomentIO (g s))
    -> Sequence f g XHRRequest
    -> (UnionsTo (Sequence f g) SEvent) (EitherBoth () (XHRResponse body))
xhr commuteF commuteG sequence =
    let -- Every time the sequence changes it means to make a request.
        -- We derive @pendings@ so that a OneLeft or Both means that a request
        -- was made.
        -- TODO would be nice to give a token instead of () so that the program
        -- could use it to cancel an in-flight request.
        pendings :: Sequence f g (EitherBoth () (XHRResponse body))
        pendings = (const (OneLeft ())) <$> sequence

        -- Every non-Nothing hit of the sequence spawns a request.
        spawns :: Sequence f g (MomentIO (SEvent (XHRResponse body), XMLHttpRequest))
        spawns = makeXHRFromRequest <$> sequence

        -- We commute the @spawns@ sequence and discard the XHR, since we don't
        -- offer a way to cancel at present.
        responseSequence :: Sequence f g (SEvent (XHRResponse body))
        responseSequence = fst <$> sequenceCommute' commuteF commuteG spawns

        -- Now to recover our responses. We switch the @responseSequence@,
        -- and we're careful to disambiguate by choosing *later* responses,
        -- as that's the nature of this function: if you make a new request
        -- while another is in flight, you will never hear the response of the
        -- old one.
        switched :: SEvent (XHRResponse body)
        switched = switch (flip const) responseSequence

        responses :: SEvent (EitherBoth () (XHRResponse body))
        responses = fmap OneRight switched

        unioner left right = case (left, right) of
            (OneLeft x, OneRight y) -> (Both x y)
            -- Other cases are in fact impossible.

    in  sequenceUnion' unioner pendings responses

-- Some tests to check that the types are computed well.
-- If you give an SBehavior as input, you get an SBehavior as output
-- (immediately there is a "request in flight" event (OneLeft ()))
-- If you give an SEvent as input, you get an SEvent as output.
test1 :: SBehavior (EitherBoth () (XHRResponse JSString))
test1 = let q = undefined :: SBehavior XHRRequest
        in  xhr (fmap Identity . runIdentity) (fmap Identity . runIdentity) q

test2 :: SEvent (EitherBoth () (XHRResponse JSString))
test2 = let q = undefined :: SEvent XHRRequest
        in  xhr (const (pure (Const ()))) (fmap Identity . runIdentity) q

-- | Make and send an XHR. You get the event giving the response, and the
--   XHR itself, in case perhaps you want to cancel it or something.
makeXHRFromRequest
    :: FromJSString body
    => XHRRequest
    -> MomentIO (SEvent (XHRResponse body), XMLHttpRequest)
makeXHRFromRequest xhrRequest = do
    xhrObject <- newXMLHttpRequest
    open xhrObject (show (xhrRequestMethod xhrRequest))
                   (xhrRequestURL xhrRequest)
                   (Just True) -- True meaning do not block
                   (Nothing :: Maybe JSString)
                   (Nothing :: Maybe JSString)
    forM_ (xhrRequestHeaders xhrRequest) (uncurry (setRequestHeader xhrObject))
    -- Seems we must actually write out the concurrency here in Haskell. Even
    -- though our XHR is asynchronous, it blocks a Haskell thread.
    thread <- case xhrRequestBody xhrRequest of
        Nothing -> liftIO (async (send xhrObject))
        Just str -> liftIO (async (sendString xhrObject str))
    (ev, fire) <- newEvent
    -- When the state changes to 4, we build an XHRResponse and fire the event.
    liftIO $ on xhrObject readyStateChange $ do
                 readyState <- getReadyState xhrObject
                 if readyState /= 4
                 then return ()
                 else do status <- getStatus xhrObject
                         responseText <- getResponseText xhrObject
                         liftIO $ fire (XHRResponse (fromIntegral status) responseText)
    return (eventToSEvent ev, xhrObject)
