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
import Control.Concurrent.Async
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
data XHRResponse body = XHRResponse {
      xhrResponseStatus :: XHRStatus
    , xhrResponseBody :: Maybe body
    }

{-
-- | The output type is EitherBoth because we need to indicate that a request
--   is launch and/or a response has been received. These could happen
--   simultaneously, although it's probably a very rare case.
xhr :: FromJSString body => EventTransformer XHRRequest (EitherBoth () (XHRResponse body))
xhr = Kleisli $ \ev -> do
           out <- eventful $ execute (makeXHRFromRequest <$> ev)
           let responses = switchE (fst <$> out)
           let ev' = const (OneLeft ()) <$> ev
           let responses' = OneRight <$> responses
           let unioner left right = case (left, right) of
                   (OneLeft x, OneRight y) -> Both x y
           return $ unionWith unioner ev' responses'
-}

-- | We choose Sequence (Maybe XHRRequest) because this allows the user to
--   immediately spawn a request (Just request |> event) or to wait and just
--   use an event (Nothing |> event).
--
--   TODO it's annoying that users who have a proper sequence (not with its
--   type parameter wrapped in Maybe) still have to check for Nothing.
--   I suppose we could solve this by typeclassing sequence.
--
--     instance IsSequence Event
--     instance IsSequence Sequence
--
--   and have xhr give back an Event if you give an Event, a Sequence if you
--   give a Sequence.
xhr
    :: forall body .
       FromJSString body
    => Sequence (Maybe XHRRequest)
    -> Sequence (Maybe (EitherBoth () (XHRResponse body)))
xhr = \sequence -> 
    let -- Every non-Nothing hit of the sequence means there's a pending request.
        pendings :: Sequence (Maybe (EitherBoth () (XHRResponse body)))
        pendings = (fmap . fmap) (const (OneLeft ())) sequence

        -- Every non-Nothing hit of the sequence also spawns a request.
        spawns :: Sequence (Maybe (MomentIO (Banana.Event (XHRResponse body), XMLHttpRequest)))
        spawns = (fmap . fmap) makeXHRFromRequest sequence

        spawns' :: Sequence (MomentIO (Maybe (Banana.Event (XHRResponse body), XMLHttpRequest)))
        spawns' = maybe (return Nothing) (fmap Just) <$> spawns

        responseSequence :: Sequence (Maybe (Banana.Event (XHRResponse body)))
        responseSequence = (fmap . fmap) fst (sequenceCommute' spawns')

        responseSequence' :: Sequence (Banana.Event (XHRResponse body))
        responseSequence' = maybe never id <$> responseSequence

        responses :: Sequence (Maybe (EitherBoth () (XHRResponse body)))
        responses = (fmap . fmap) (OneRight) (sequenceSwitch' responseSequence')

        unioner left right = case (left, right) of
            (Just (OneLeft x), Just (OneRight y)) -> Just (Both x y)
            (Nothing, r) -> r
            (l, Nothing) -> l

    in  sequenceUnion unioner pendings responses

makeXHRFromRequest :: FromJSString body => XHRRequest -> MomentIO (Banana.Event (XHRResponse body), XMLHttpRequest)
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
    liftIO $ on xhrObject readyStateChange $ do
                 readyState <- getReadyState xhrObject
                 if readyState /= 4
                 then return ()
                 else do status <- getStatus xhrObject
                         responseText <- getResponseText xhrObject
                         liftIO $ fire (XHRResponse (fromIntegral status) responseText)
    return (ev, xhrObject)

type XHRAbort = Unique
type XHRPending = Unique

xhrResponses :: Banana.Event (EitherBoth XHRPending t) -> Banana.Event t 
xhrResponses = filterJust . fmap (bifoldl (const) (const Just) Nothing)

{-
-- | Input: either abort an existing request or start a new request.
--   Output: a pending request, which can be fed back in to abort it, or
--   a response.
xhr'
    :: EventTransformer (EitherBoth XHRAbort XHRRequest)
                        (EitherBoth XHRPending XHRResponse)
xhr' = Kleisli $ \ev -> do

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

-}
