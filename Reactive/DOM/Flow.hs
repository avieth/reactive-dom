{-|
Module      : Reactive.DOM.Flow
Description : Definition of Flows.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Reactive.DOM.Flow (

      Flow(Flow)
    , pureFlow
    , impureFlow
    , varyingFlow
    , widgetFlow
    , widgetFlow1
    , flowMap
    , CompleteFlow
    , runFlow

    ) where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Category
import Control.Arrow
import Data.Void
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Monoid (mempty)
import Data.Algebraic.Index
import Data.Algebraic.Sum
import Data.Algebraic.Product hiding (Component)
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Widget

-- | Description of a user interface flow: taking an s, producing a t, with
--   a side-channel producing an o.
data Flow o s t where
    -- | Do some MomentIO computation and either pass a value immediately, or
    --   show a Widget and pass the value through an Event.
    --   This constructor gives a *lot* of leeway: you can do any IO you
    --   want. But be careful! A useful Flow should never appear to be blocked.
    --   So don't put, for example, a synchronous HTTP request in here. Instead,
    --   throw in a Widget with an Event which fires when the request is done.
    Flow :: (s -> MomentIO (Either t (Sequence (Widget o, Event t)))) -> Flow o s t
    -- | A time-varying Flow. The Behavior must be derived from the Event
    --   via stepper.
    FlowVarying :: (Behavior (Flow o s t), Event (Flow o s t)) -> Flow o s t
    -- | Symbolic composition.
    FlowCompose :: Flow o u t -> Flow o s u -> Flow o s t
    -- | Symbol first, for Arrow.
    FlowFirst :: Flow o s t -> Flow o (s, c) (t, c)
    -- | Symbol left, for ArrowChoice.
    FlowLeft :: Flow o s t -> Flow o (Either s c) (Either t c)

-- | A @CompleteFlow@ is a neverending @Flow@. Give an input @s@ and you'll
--   have a non-stop user interface. Only this kind of @Flow@ can be run to
--   produce a @VirtualElement@ (see @runFlow@).
type CompleteFlow o s = Flow o s Void

instance Category (Flow o) where
    id = Flow $ pure . Left
    (.) = FlowCompose

instance Arrow (Flow o) where
    arr = pureFlow
    first = FlowFirst

instance ArrowChoice (Flow o) where
    left = FlowLeft

-- | Make a Flow which shows a Sequence of Widgets.
widgetFlow :: (s -> MomentIO (Sequence (Widget o, Event t))) -> Flow o s t
widgetFlow mk = Flow $ fmap Right . mk

-- | Make a Flow which shows one Widget.
widgetFlow1 :: (s -> MomentIO (Widget o, Event t)) -> Flow o s t
widgetFlow1 mk = widgetFlow (fmap mkSingle . mk)
  where
    mkSingle (x, y) = (x, y) |> never

-- | Make a flow which shows no Widget, just runs a pure function.
--   pureFlow = arr
pureFlow :: (s -> t) -> Flow o s t
pureFlow f = Flow $ pure . Left . f

-- | Make a flow which shows no Widget, just runs an impure function.
--   Careful: you should not do anything blocking here, or the Flow will be
--   interrupted (will not flow continuously from Widget to Widget).
impureFlow :: (s -> MomentIO t) -> Flow o s t
impureFlow f = Flow $ fmap Left . f

varyingFlow :: Sequence (Flow o s t) -> MomentIO (Flow o s t)
varyingFlow seqnc = do
    (first, rest) <- runSequence seqnc
    b <- stepper first rest
    pure $ FlowVarying (b, rest)

flowMap :: (o -> o') -> Flow o s t -> Flow o' s t
flowMap f flow = case flow of
    FlowCompose l r -> FlowCompose (flowMap f l) (flowMap f r)
    FlowFirst fst -> FlowFirst (flowMap f fst)
    FlowLeft left -> FlowLeft (flowMap f left)
    Flow mk -> Flow $ \s -> do
        choice <- mk s
        case choice of
            Left t -> pure . Left $ t
            Right seqnc -> pure . Right $ ((\(x, y) -> (f <$> x, y)) <$> seqnc)

data NextComponent o where
    FoundComponent
        :: (t -> MomentIO (Sequence (NextComponent o)))
        -> Sequence (Widget o, Event t)
        -> NextComponent o

runNextComponent
    :: forall o .
       NextComponent o
    -> Sequence (Widget o, Event (MomentIO (Sequence (NextComponent o))))
runNextComponent (FoundComponent next seqnc) =
    let runIt (widget, ev) = (widget, next <$> ev)
    in  fmap runIt seqnc

runNextComponents
    :: forall o .
       Sequence (NextComponent o)
    -> MomentIO (Sequence (Widget o))
runNextComponents componentSequence = do

    let seqnc :: Sequence (Widget o, Event (MomentIO (Sequence (NextComponent o))))
        seqnc = sequenceSwitch (runNextComponent <$> componentSequence)

    let initialWidgets :: Sequence (Widget o)
        initialWidgets = fst <$> seqnc

    let eventSequence :: Sequence (MomentIO (Event (Sequence (NextComponent o))))
        eventSequence = execute . snd <$> seqnc

    commutedEventSequence :: Sequence (Event (Sequence (NextComponent o)))
        <- sequenceCommute eventSequence

    commutedEvent :: Event (Sequence (NextComponent o))
        <- sequenceSwitchE commutedEventSequence

    let nexts :: Event (MomentIO (Sequence (Widget o)))
        nexts = runNextComponents <$> commutedEvent

    commutedNexts :: Event (Sequence (Widget o))
        <- execute nexts

    let switched = sequenceSwitch (initialWidgets |> commutedNexts)

    pure switched

-- | If we can come up with the next component from t, then we can come up
--   with the next component from s: run the flow until a Flow is found, at
--   which point we have a Widget and an Event.
--
--   Note that if t ~ Void then we can use absurd for the k parameter.
nextComponentK
    :: forall o s t .
       Flow o s t
    -> (t -> MomentIO (Sequence (NextComponent o)))
    -> (s -> MomentIO (Sequence (NextComponent o)))
nextComponentK flow k = case flow of
    Flow mk -> \s -> do choice <- mk s
                        case choice of
                            Left t -> k t
                            Right ws -> pure (always (FoundComponent k ws))
    FlowVarying (b, ev) -> \s -> do
        current :: Flow o s t <- valueB b
        let seqnc :: Sequence (Flow o s t)
            seqnc = rstepper current ev
        let nexts :: Sequence (MomentIO (Sequence (NextComponent o)))
            nexts = (\flow -> nextComponentK flow k s) <$> seqnc
        commuted <- sequenceCommute nexts
        let switched = sequenceSwitch commuted
        pure switched
    FlowCompose left right ->
        let k' = nextComponentK left k
        in  nextComponentK right k'
    FlowFirst subFlow -> \(s, c) -> nextComponentK subFlow (\t -> k (t, c)) s
    FlowLeft subFlow -> \sum -> case sum of
        Left x -> nextComponentK subFlow (\t -> k (Left t)) x
        Right y -> k (Right y)

flowSequence
    :: Flow o s t
    -> (t -> MomentIO (Sequence (NextComponent o)))
    -> s
    -> MomentIO (Sequence (Widget o))
flowSequence flow k s = do
    nextComponent <- nextComponentK flow k s 
    runNextComponents nextComponent

-- | Produce a virtual element which shows a complete flow, along with the
--   switched output.
runFlow
    :: Flow o s Void
    -> s 
    -> MomentIO (Widget (Sequence o))
runFlow flow s = do
    widgets <- flowSequence flow absurd s
    let seqnc = runWidget <$> widgets
    let outs = fst <$> seqnc
    let velems = snd <$> seqnc
    velem <- virtualElement ("div")
                            (always mempty)
                            (always mempty)
                            (always mempty)
                            (pure . node <$> velems)
    pure $ Widget (outs, velem)
