{-|
Module      : Reactive.DOM.Xhr
Description : Definition of reactive Xhr-related things.
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

module Reactive.DOM.Xhr (

      XhrHandler
    , xhrHandler
    , xhrParallel
    , xhrParallelTraverse

    , xhr1
    , xhrMany
    , xhrLimited

    , XhrURL
    , XhrStatus
    , XhrHeaders
    , XhrMethod(..)
    , XhrRequest(..)
    , XhrResponse(..)

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
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Profunctor
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List.NonEmpty
import Data.Maybe (isNothing, isJust, mapMaybe)
import Data.JSString.Text
import GHCJS.Types
import GHCJS.DOM.Types
import GHCJS.DOM.Element as Element
import GHCJS.DOM.XMLHttpRequest as Xhr
import GHCJS.DOM.JSFFI.XMLHttpRequest as Xhr hiding (send, cancel)
import GHCJS.DOM.EventM
import Reactive.Banana.Combinators as Banana
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Debug.Trace

-- | An XMLHttpRequest, with extra data for equality and ordering, as well as
--   a response event.
newtype Xhr = Xhr {
      runXhr
          :: ( XMLHttpRequest
             , Unique
             , Banana.Event XhrResponse
             )
    }

instance Eq Xhr where
    (Xhr (_, u1, _)) == (Xhr (_, u2, _)) = u1 == u2

instance Ord Xhr where
    (Xhr (_, u1, _)) `compare` (Xhr (_, u2, _)) = u1 `compare` u2

instance Show Xhr where
    show (Xhr (_, u, _)) = mconcat ["Xhr ", show (hashUnique u)]

cancelXhr :: Xhr -> IO ()
cancelXhr xhr@(Xhr (xhr_, _, _)) = do
    putStrLn ("cancelXhr : cancelling " ++ show xhr)
    Xhr.abort xhr_
    pure ()

type XhrURL = T.Text

type XhrStatus = Int

type XhrHeaders = [(T.Text, T.Text)]

data XhrMethod = GET | PUT | POST | DELETE

instance Show XhrMethod where
    show method = case method of
        GET -> "GET"
        PUT -> "PUT"
        POST -> "POST"
        DELETE -> "DELETE"

data XhrRequest = XhrRequest {
      xhrRequestMethod :: XhrMethod
    , xhrRequestURL :: XhrURL
    , xhrRequestHeaders :: XhrHeaders
    , xhrRequestBody :: Maybe TL.Text
    , xhrRequestAuth :: Maybe (T.Text, T.Text)
    }

deriving instance Show XhrRequest

data XhrResponse = XhrResponse {
      xhrResponseStatus :: XhrStatus
    , xhrResponseHeaders :: XhrHeaders
    , xhrResponseBody :: Maybe TL.Text
    }

deriving instance Show XhrResponse

-- | Make and send an Xhr. You get the event giving the response, and the
--   Xhr itself, in case perhaps you want to cancel it.
--
--   TODO There's something wrong in here w.r.t. undefined/error values in the
--   body. If we give one, the Xhr dies, but the error doesn't propagate up to
--   the main thread, which can be rather confusing.
makeXhrFromRequest
    :: XhrRequest
    -> MomentIO Xhr
makeXhrFromRequest xhrRequest = do
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
    --          "makeXhrFromRequest "
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
            --putStrLn (mconcat ["makeXhrFromRequest empty body"])
            async (send xhrObject)
        Just txt -> liftIO $ do
            --putStrLn (mconcat ["makeXhrFromRequest body ", show txt])
            --putStrLn (mconcat ["makeXhrFromRequest body"])
            async $ do
                sendString xhrObject (lazyTextToJSString txt)
                --putStrLn (mconcat ["makeXhrFromRequest body sent"])

    uniq <- liftIO newUnique
    (evValue, fireValue) <- newEvent
    let xhr = Xhr (xhrObject, uniq, evValue)
    liftIO (putStrLn ("makeXhrFromRequest : spawning " ++ show xhr))

    -- When the state changes to 4, we build an XhrResponse and fire the event.
    liftIO $ on xhrObject readyStateChange $ do
                 -- Note: it is observed that, if an Xhr is aborted, then the
                 -- ready state goes to 4 but the status code goes to 0. Thus
                 -- we check both.
                 readyState <- getReadyState xhrObject
                 status <- getStatus xhrObject

                 --let traceString = mconcat [
                 --          "makeXhrFromRequest changed ready state to "
                 --        , show readyState
                 --        ]
                 --liftIO (putStrLn traceString)
                 
                 if readyState /= 4 || status == 0
                 then return ()
                 else do -- getAllResponseHeaders just gives a string; we've got
                         -- to separate them into a list. MDN says they're
                         -- to be separated by CRLF.
                         Just responseHeaders <- getAllResponseHeaders xhrObject
                         let headers = marshallResponseHeaders responseHeaders
                         when (not (corsPreflight headers)) $ do
                             responseText <- getResponseText xhrObject
                             let response = XhrResponse (fromIntegral status)
                                                        (headers)
                                                        (lazyTextFromJSString <$> responseText)
                             liftIO $ putStrLn ("Xhr yielding response for : " ++ show xhr)
                             liftIO $ fireValue response
    pure xhr

-- | Use the headers to identify a CORS preflight request.
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

-- | An ArrowApply formulation of an Xhr handler: essentially a function
--     s -> (XhrRequest, XhrResponse -> t)
--   along with formal combinations to make it an ArrowApply and to allow for
--   explicit parallelism.
data XhrHandler s t where
    XhrHandlerArr :: (s -> t) -> XhrHandler s t
    XhrHandlerComp :: XhrHandler u t -> XhrHandler s u -> XhrHandler s t
    XhrHandlerFirst :: XhrHandler s t -> XhrHandler (s, c) (t, c)
    XhrHandlerLeft :: XhrHandler s t -> XhrHandler (Either s c) (Either t c)
    XhrHandlerApp :: XhrHandler (XhrHandler s t, s) t
    XhrHandlerParallel :: XhrHandler s1 t1 -> XhrHandler s2 t2 -> XhrHandler (s1, s2) (t1, t2)
    XhrHandlerOne :: (s -> (XhrRequest, XhrResponse -> t)) -> XhrHandler s t

instance Category XhrHandler where
    id = arr id
    (.) = XhrHandlerComp

instance Arrow XhrHandler where
    arr = XhrHandlerArr
    first = XhrHandlerFirst

instance ArrowChoice XhrHandler where
    left = XhrHandlerLeft

instance ArrowApply XhrHandler where
    app = XhrHandlerApp

instance Profunctor XhrHandler where
    dimap l r handler = arr l >>> handler >>> arr r

instance Functor (XhrHandler s) where
    fmap = rmap

-- | <*> runs each Xhr in parallel.
instance Applicative (XhrHandler s) where
    pure = arr . const
    hf <*> hx = arr (\s -> (s, s)) >>> XhrHandlerParallel hf hx >>> arr (uncurry ($))

-- | An atomic Xhr handler: say how to make a request and how to handle a
--   response.
xhrHandler :: (s -> (XhrRequest, XhrResponse -> t)) -> XhrHandler s t
xhrHandler = XhrHandlerOne

-- | Indicate that two XhrHandlers should be run parallel. Actually, their
--   pure function components will be run sequentially, but Xhrs spawned
--   by either will not block the other.
xhrParallel :: XhrHandler s1 t1 -> XhrHandler s2 t2 -> XhrHandler (s1, s2) (t1, t2)
xhrParallel = XhrHandlerParallel

-- | Wind an XhrHandler through a traversable, running each in parallel.
--   This is actually a special case of a general construction for any
--   ArrowApply, which is a Monad and therefore can be used like an
--   Applicative via sequenceA.
xhrParallelTraverse
    :: forall s t f .
       ( Traversable f )
    => XhrHandler s t
    -> XhrHandler (f s) (f t)
xhrParallelTraverse handler = proc it -> do
    let it' :: f (XhrHandler () t)
        it' = fmap (\x -> arr (const x) >>> handler) it
    let traversed :: XhrHandler () (f t)
        traversed = sequenceA it'
    app -< (traversed, ())

data XhrChange = Spawn Xhr | Finish Xhr
  deriving (Show)

pickSpawn :: XhrChange -> Maybe Xhr
pickSpawn (Spawn x) = Just x
pickSpawn _ = Nothing

pickFinish :: XhrChange -> Maybe Xhr
pickFinish (Finish x) = Just x
pickFinish _ = Nothing

-- TODO use a set instead.
mergeXhrChanges :: NonEmpty XhrChange -> [Xhr] -> [Xhr]
mergeXhrChanges news olds =
    let list = toList news
        spawns = mapMaybe pickSpawn list
        finishes = mapMaybe pickFinish list
    in     (Prelude.filter (\x -> not (elem x olds)) spawns)
        ++ (Prelude.filter (\x -> not (elem x finishes)) olds)

-- | A tool for running XhrHandlers. It's either an immediate value (no Xhrs
--   created) or a list of Xhrs spawned, and events to give the final value
--   and the changes to the active Xhrs.
newtype XhrContinuation t = XhrContinuation {
      runXhrContinuation :: Either t (MomentIO (Banana.Event t, NonEmpty Xhr, Banana.Event (NonEmpty XhrChange)))
    }

instance Functor XhrContinuation where
    fmap f (XhrContinuation choice) = XhrContinuation $ case choice of
        Left t -> Left (f t)
        Right m -> Right $ do
            (evT, xhrs, evXhrs) <- m
            pure (fmap f evT, xhrs, evXhrs)

-- | A continuation which immediately yields its input.
trivialXhrContinuation :: t -> XhrContinuation t
trivialXhrContinuation = XhrContinuation . Left

-- | The sequential composition of two XhrContinuations: run the first, feed
--   its value to the second, merging their Xhr events.
sequenceXhrContinuations
    :: forall s t .
       XhrContinuation s
    -> (s -> XhrContinuation t)
    -> XhrContinuation t
sequenceXhrContinuations (XhrContinuation x) k = case x of
    Left s -> k s
    Right m -> XhrContinuation . Right $ do
        -- Something we know: evXhrs will not fire after evS.
        (evS, xhrs, evXhrsS) <- m
        let evNext :: Banana.Event (Either t (MomentIO (Banana.Event t, NonEmpty Xhr, Banana.Event (NonEmpty XhrChange))))
            evNext = runXhrContinuation . k <$> evS
        -- Pick the immediate case.
        let evNextImmediate :: Banana.Event t
            evNextImmediate = filterJust (either Just (const Nothing) <$> evNext)
        -- Pick the delayed case.
        evNextDelayed :: Banana.Event (Banana.Event t, NonEmpty Xhr, Banana.Event (NonEmpty XhrChange))
            <- execute (filterJust (either (const Nothing) Just <$> evNext))
        -- The value event. Either it comes immediately from the next one (next
        -- is not an Xhr, but a pure function) or the next one has to wait for
        -- it too.
        let ev :: Banana.Event t
            ev = unionWith const
                           evNextImmediate
                           (switchE ((\(x,_,_) -> x) <$> evNextDelayed))
        -- The xhrs event: grab the initial Xhrs of the next one, then the
        -- remaining xhrs.
        -- The initial Xhrs are of course prefixed with Spawn.
        let evNextXhrsInitial :: Banana.Event (NonEmpty XhrChange)
            evNextXhrsInitial = ((\(_,x,_) -> Spawn <$> x) <$> evNextDelayed)
        let evNextXhrsDelayed :: Banana.Event (NonEmpty XhrChange)
            evNextXhrsDelayed = switchE ((\(_,_,x) -> x) <$> evNextDelayed)
        let evXhrs :: Banana.Event (NonEmpty XhrChange)
            evXhrs = unionWith (<>) evNextXhrsDelayed evNextXhrsInitial
        -- Don't forget to include the local xhr changes with the remaining
        -- ones.
        pure (ev, xhrs, unionWith (<>) evXhrsS evXhrs)

-- | The parallel composition of two XhrContinuations: run each of them, and
--   merge their Xhrs and events.
parallelXhrContinuations
    :: XhrContinuation t1
    -> XhrContinuation t2
    -> XhrContinuation (t1, t2)
parallelXhrContinuations (XhrContinuation left) (XhrContinuation right) = case left of
    Left t1 -> fmap ((,) t1) (XhrContinuation right)
    Right mleft -> XhrContinuation . Right $ do
        (evT1, xhrsLeft, evXhrsLeft) <- mleft
        case right of
            Left t2 -> pure (flip (,) t2 <$> evT1, xhrsLeft, evXhrsLeft)
            Right mright -> do
                -- Here's why we need to be in Moment: we have events for
                -- the xhrs and values of both continuations. In order to
                -- join them we need steppers. If the left value fires first,
                -- for instance, we must remember it in a behavior so that we
                -- can fire the tupled event when the right value comes (and
                -- same vice-versa).
                (evT2, xhrsRight, evXhrsRight) <- mright
                beT1 <- stepper Nothing (Just <$> evT1)
                beT2 <- stepper Nothing (Just <$> evT2)
                beXhrsLeft <- stepper Nothing (Just <$> evXhrsLeft)
                beXhrsRight <- stepper Nothing (Just <$> evXhrsRight)
                let evJoinLeft = filterJust $ (\x y -> (,) <$> x <*> pure y) <$> beT1 <@> evT2
                let evJoinRight = filterJust $ (\x y -> flip (,) <$> x <*> pure y) <$> beT2 <@> evT1
                let evJoinXhrsLeft = filterJust $ (\x y -> (<>) <$> x <*> pure y) <$> beXhrsLeft <@> evXhrsRight
                let evJoinXhrsRight = filterJust $ (\x y -> (<>) <$> x <*> pure y) <$> beXhrsRight <@> evXhrsLeft
                let ev = unionWith const evJoinLeft evJoinRight
                let xhrs = xhrsLeft <> xhrsRight
                let evXhrs = unionWith (<>) evJoinXhrsLeft evJoinXhrsRight
                pure (ev, xhrs, evXhrs)

-- | Running an XhrHandler will always give, eventually, a value of type t,
--   even if any or all of the Xhrs fail; the handlers must be total functions
--   of XhrResponse.
runXhrHandler
    :: forall s t r .
       XhrHandler s t
    -> (t -> XhrContinuation r)
    -> (s -> XhrContinuation r)
runXhrHandler handler k = case handler of
    XhrHandlerArr f -> k . f
    XhrHandlerComp (left :: XhrHandler u t) (right :: XhrHandler s u) ->
        let k' :: u -> XhrContinuation r
            k' = runXhrHandler left k
        in  runXhrHandler right k'
    XhrHandlerFirst handler -> \(s, c) ->
        runXhrHandler handler (\t -> k (t, c)) s
    XhrHandlerLeft handler -> \choice -> case choice of
        Left s -> runXhrHandler handler (\t -> k (Left t)) s
        Right c -> k (Right c)
    XhrHandlerApp -> \(handler, s) ->
        runXhrHandler handler k s
    XhrHandlerParallel (left :: XhrHandler s1 t1) (right :: XhrHandler s2 t2) -> \(s1, s2) ->
        -- We make a parallel XhrContinuation. for left and right...
        let left' :: XhrContinuation t1
            left' = runXhrHandler left trivialXhrContinuation s1
            right' :: XhrContinuation t2
            right' = runXhrHandler right trivialXhrContinuation s2
            parallel :: XhrContinuation (t1, t2)
            parallel = parallelXhrContinuations left' right'
        -- ... and then sequence the continuation after it.
        in  sequenceXhrContinuations parallel k
    XhrHandlerOne mk -> \s ->
        let (req, mkres) = mk s
            thisOne = XhrContinuation . Right $ do
                xhr@(Xhr (xhrObj, _, evFinish))
                    <- makeXhrFromRequest req
                let ev = mkres <$> evFinish
                let xhrChanges = const (Finish xhr :| []) <$> evFinish
                pure (ev, xhr :| [], xhrChanges)
        in  sequenceXhrContinuations thisOne k

-- | Convenient way of going straight to a MomentIO: make an XhrContinuation and
--   then run it.
evalXhrHandler
    :: XhrHandler s t
    -> s
    -> MomentIO (Either t (Banana.Event t, NonEmpty Xhr, Banana.Event (NonEmpty XhrChange)))
evalXhrHandler handler s = do
    let cont = runXhrHandler handler trivialXhrContinuation s
    case runXhrContinuation cont of
        Left t -> pure (Left t)
        Right m -> Right <$> m

-- | Run an XhrHandler immediately. 
xhr1 :: XhrHandler s t -> s -> MomentIO (Eventually t)
xhr1 handler s = do
    let cont = runXhrHandler handler trivialXhrContinuation s
    case runXhrContinuation cont of
        Left t -> pure (immediately t)
        Right m -> do
            (ev, _, _) <- m
            pure (delayed ev)

-- | Run an XhrHandler whenever an event fires to give it input. If it fires
--   twice before the XhrHandler of the first has finished, then that
--   XhrHandler's Xhrs are cancelled and its output forgotten.
xhrMany
    :: forall s t .
       XhrHandler s t
    -> Banana.Event s
    -> Compose MomentIO Banana.Event t
xhrMany handler sev = Compose $ mdo

    continuations :: Banana.Event (Either t (Banana.Event t, NonEmpty Xhr, Banana.Event (NonEmpty XhrChange)))
        <- execute (runXhrHandlerAndCancel handler <$> currentXhrs <@> sev)

    let immediates :: Banana.Event t
        immediates = filterJust (either Just (const Nothing) <$> continuations)

    let delayeds :: Banana.Event (Banana.Event t, NonEmpty Xhr, Banana.Event (NonEmpty XhrChange))
        delayeds = filterJust (either (const Nothing) Just <$> continuations)

    let delayedValues :: Banana.Event t
        delayedValues = switchE ((\(x,_,_) -> x) <$> delayeds)

    -- The Xhrs that are spawned immediately (simultaneously with the input
    -- event sev).
    let firstXhrs :: Banana.Event (NonEmpty Xhr)
        firstXhrs = (\(_,x,_) -> x) <$> delayeds

    -- The updates to the Xhrs; switched whenever the input event sev fires.
    let changeXhrs :: Banana.Event (NonEmpty XhrChange)
        changeXhrs = switchE ((\(_,_,x) -> x) <$> delayeds)

    let updateCurrentXhrs :: Banana.Event [Xhr]
        updateCurrentXhrs = unionWith const
                                      (toList <$> firstXhrs)
                                      (flip mergeXhrChanges <$> currentXhrs <@> changeXhrs)

    -- Whenever sev fires, everything in currentXhrs will be cancelled, so we
    -- want to forget about them. Simultaneous with sev is the firstXhrs event
    -- giving the new spawns corresponding to sev, so we just make a union and
    -- favour that.
    currentXhrs :: Behavior [Xhr]
        <- stepper [] updateCurrentXhrs

    let values = unionWith const immediates delayedValues

    pure values

  where

    runXhrHandlerAndCancel
        :: XhrHandler s t
        -> [Xhr]
        -> s
        -> MomentIO (Either t (Banana.Event t, NonEmpty Xhr, Banana.Event (NonEmpty XhrChange)))
    runXhrHandlerAndCancel handler xhrs s = do
        _ <- cancelInFlight xhrs
        evalXhrHandler handler s

    cancelInFlight :: [Xhr] -> MomentIO ()
    cancelInFlight xhrs = liftIO $ forM_ xhrs cancelXhr

-- | Like xhrMany except that we never cancel the current request. Instead, when
--   the event fires while there's a pending Xhr, we queue up the request for
--   the latest value from the event. We also feed each handler the output value
--   of the last handler to run (it's gotta be a monoid, so we can give mempty
--   to the first request).
--   Your XhrHandler should give Nothing in the second component if it doesn't
--   want to trigger the output event.
xhrLimited
    :: forall r s t .
       ( Monoid r )
    => XhrHandler (r, s) (r, Maybe t)
    -> Banana.Event s
    -> Compose MomentIO Banana.Event t
xhrLimited handler sev = Compose $ mdo

    -- Grab the continuations, from which we shall derive our values.
    continuations :: Banana.Event (Either (r, Maybe t) (Banana.Event (r, Maybe t), NonEmpty Xhr, Banana.Event (NonEmpty XhrChange)))
        <- execute (evalXhrHandler handler <$> ((,) <$> mostRecentResponse <@> spawns))

    let immediates :: Banana.Event (r, Maybe t)
        immediates = filterJust (either Just (const Nothing) <$> continuations)

    let delayeds :: Banana.Event (Banana.Event (r, Maybe t), NonEmpty Xhr, Banana.Event (NonEmpty XhrChange))
        delayeds = filterJust (either (const Nothing) Just <$> continuations)

    let values :: Banana.Event (r, Maybe t)
        values = unionWith const immediates (switchE ((\(x,_,_) -> x) <$> delayeds))

    -- Create a gate: whenever a value is returned it opens (True) and whenever
    -- a new request is spawned it closes (False).
    let gateChange :: Banana.Event Bool
        gateChange = unionWith const (const True <$> values) (const False <$> sev)
    gate :: Behavior Bool
        <- stepper True gateChange

    -- Fires when we should spawn one: sev when the gate is open, or the
    -- queued thing when we get a value back, if there is something queued.
    let spawns :: Banana.Event s
        spawns = unionWith const (whenE gate sev) (filterJust (queued <@ values))
    queued <- stepper Nothing (Just <$> (whenE (not <$> gate) sev))

    mostRecentResponse :: Behavior r
        <- stepper mempty (fst <$> values)

    pure (filterJust (snd <$> values))
