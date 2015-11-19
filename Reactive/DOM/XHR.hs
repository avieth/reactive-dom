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

module Reactive.DOM.XHR where

import Control.Arrow
import Control.Monad (forM_)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class
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
import GHCJS.DOM.XMLHttpRequest as XHR hiding (send)
import GHCJS.DOM.JSFFI.XMLHttpRequest as XHR
import GHCJS.DOM.EventM
import Reactive.Banana.Combinators as Banana
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.Eventful
import Reactive.EventTransformer

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

type XHRAbort = Unique
type XHRPending = Unique

xhrResponses :: Banana.Event (EitherBoth XHRPending t) -> Banana.Event t 
xhrResponses = filterJust . fmap (bifoldl (const) (const Just) Nothing)

xhr' :: XHRRequest -> MomentIO (Banana.Event XHRResponse)
xhr' = fmap fst . makeXHRFromRequest

-- | Input: either abort an existing request or start a new request.
--   Output: a pending request, which can be fed back in to abort it, or
--   a response.
xhr
    :: EventTransformer (EitherBoth XHRAbort XHRRequest)
                        (EitherBoth XHRPending XHRResponse)
xhr = Kleisli $ \ev -> do

          xhrs :: IORef (M.Map Unique XMLHttpRequest) <- liftIO $ newIORef M.empty

          -- When an event comes in, we get either
          --     () in case it was an abort.
          --     immediately a Unique indifying the new request, and an event
          --         with this request's response.
          --     or both in case it was both.
          let req :: EitherBoth XHRAbort XHRRequest -> MomentIO (EitherBoth () (Unique, Banana.Event XHRResponse))
              req = bitraverse (cancelXHR xhrs) (spawnXHR xhrs)
          out :: Banana.Event (EitherBoth () (Unique, Banana.Event XHRResponse))
              <- eventful $ execute (req <$> ev)

          -- Now we use the event @out@ to produce something of type
          -- 
          --     Event (EitherBoth XHRPending XHRResponse)
          --
          -- This is obtained by discarding the ()'s to get
          --
          --     Event (Unique, Banana.Event XHRResponse)
          --
          -- splitting this into two distinct events, switching the second,
          -- and then unioning into an EitherBoth.
          let pickSecond :: EitherBoth s t -> Maybe t
              pickSecond e = case e of
                  OneLeft _ -> Nothing
                  OneRight x -> Just x
                  Both _ x -> Just x

          let filtered :: Banana.Event (Unique, Banana.Event XHRResponse)
              filtered = filterJust (pickSecond <$> out)

          let responses :: Banana.Event XHRResponse
              responses = switchE (snd <$> filtered)

          let pendings :: Banana.Event Unique
              pendings = fst <$> filtered

          let unioner
                  :: EitherBoth a b
                  -> EitherBoth a b
                  -> EitherBoth a b
              unioner l r = case (l, r) of
                  (OneLeft l', OneRight r') -> Both l' r'
                  -- Impossible. Notice the use of unioner: left argument is
                  -- always OneLeft, second is always OneRight.
                  -- Wish I didn't have to give a partial function, but since
                  -- unionWith is very strict in its type parameters, I'm out
                  -- of options.
                  _ -> undefined

          let boths :: Banana.Event (EitherBoth Unique XHRResponse)
              boths = unionWith unioner (OneLeft <$> pendings) (OneRight <$> responses)

          return boths

cancelXHR :: IORef (M.Map Unique XMLHttpRequest) -> XHRAbort -> MomentIO ()
cancelXHR ref unique = do
    m <- liftIO $ readIORef ref
    case M.lookup unique m of
        Nothing -> return ()
        Just xhr -> do XHR.abort xhr
                       liftIO $ writeIORef ref (M.delete unique m)

spawnXHR
    :: IORef (M.Map Unique XMLHttpRequest)
    -> XHRRequest
    -> MomentIO (Unique, Banana.Event XHRResponse)
spawnXHR ref req = do
    u <- liftIO newUnique
    (ev, xhr) <- makeXHRFromRequest req
    liftIO $ modifyIORef ref (M.insert u xhr)
    return (u, ev)

makeXHRFromRequest :: XHRRequest -> MomentIO (Banana.Event XHRResponse, XMLHttpRequest)
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
    return (ev, xhrObject)
