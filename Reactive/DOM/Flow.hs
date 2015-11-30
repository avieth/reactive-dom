{-|
Module      : Reactive.DOM.Flow
Description : Definition of Flow for component flows.
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

module Reactive.DOM.Flow (

      Flow
    , CompleteFlow
    , flow
    , inertFlow
    , flowMap
    , runFlow
    , flowBehavior

    ) where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Category
import Control.Arrow
import Data.Void
import Data.Functor.Identity
import Data.Profunctor
import Data.Monoid (mempty)
import Data.Algebraic.Index
import Data.Algebraic.Sum
import Data.Algebraic.Product hiding (Component)
import Reactive.Banana.Frameworks (MomentIO)
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Component

-- | A @Flow f g o s t@ describes a sequence of components, each of which
--   produces an event which contains the input to the next, as well as
--   some @Sequence f g o@.
data Flow f g o s t where
    FlowArr :: (s -> t) -> Flow f g o s t
    FlowCompose :: Flow f g o u t -> Flow f g o s u -> Flow f g o s t
    FlowFirst :: Flow f g o s t -> Flow f g o (s, c) (t, c)
    FlowLeft :: Flow f g o s t -> Flow f g o (Either s c) (Either t c)
    FlowComponent :: Component s (Sequence f g o, SEvent t) -> Flow f g o s t

-- | A @CompleteFlow s@ is a neverending @Flow@. Give an input @s@ and you'll
--   have a non-stop user interface. Only this kind of @Flow@ can be run to
--   produce a @VirtualElement@ (see @runFlow@).
type CompleteFlow f g o s = Flow f g o s Void

instance Category (Flow f g o) where
    id = FlowArr id
    (.) = FlowCompose

instance Arrow (Flow f g o) where
    arr = FlowArr
    first = FlowFirst

instance ArrowChoice (Flow f g o) where
    left = FlowLeft

instance
    ( Switchable Identity f Identity g
    , SwitchesTo (SBehavior (Sequence f g o)) ~ Sequence f g o
    , Functor f
    , Functor g
    ) => IsComponent (Flow f g o s Void)
  where
    type ComponentInputT (Flow f g o s Void) = s
    type ComponentOutputT (Flow f g o s Void) = Sequence f g o
    makeComponent flow s = runKleisli (runFlow flow) s

flow :: Component s (Sequence f g o, SEvent t) -> Flow f g o s t
flow = FlowComponent

-- | Take any component and make it flow which never advances.
--   Specialize t to Void and you've got a complete flow.
inertFlow :: Sequence f g o -> Component s t -> Flow f g o s t
inertFlow sequence = FlowComponent . rmap (const (sequence, never))

-- | Alter the common result type of the flow, result type being the third
--   type parameter. Not to be confused with the output type.
flowMap
    :: (Functor f, Functor g)
    => (Sequence f g o -> Sequence f' g' o')
    -> Flow f g o s t
    -> Flow f' g' o' s t
flowMap f term = case term of
    FlowArr f -> FlowArr f
    FlowCompose left right -> FlowCompose (flowMap f left) (flowMap f right)
    FlowFirst sub -> FlowFirst (flowMap f sub)
    FlowLeft sub -> FlowLeft (flowMap f sub)
    FlowComponent component -> FlowComponent (rmap (\(l, r) -> (f l, r)) component)

-- Here's what we need.
-- The idea is to produce the next component from a Flow.
-- When we flip t ~ Void we will find that we can always produce at least
-- one, since 3 of 5 constructors are ruled out.
--
-- Here we bundle all the information needed to run the component.
data NextComponent f g o where
    FoundComponent
        :: Component s (Sequence f g o, SEvent t)
        -> s
        -> (t -> NextComponent f g o)
        -> NextComponent f g o

runNextComponent
    :: forall f g o .
       NextComponent f g o
    -> MomentIO ((Sequence f g o, SEvent (NextComponent f g o)), VirtualElement Identity)
runNextComponent (FoundComponent component input next) = do
    ((output, event), velem) <- runComponent component input
    pure ((output, next <$> event), velem)

runNextComponents
    :: forall f g o .
       NextComponent f g o
    -> MomentIO (SBehavior (Sequence f g o, VirtualElement Identity))
runNextComponents component = mdo

    initial :: ((Sequence f g o, SEvent (NextComponent f g o)), VirtualElement Identity)
        <- runNextComponent component

    let outputBehavior :: SBehavior ((Sequence f g o, SEvent (NextComponent f g o)), VirtualElement Identity)
        outputBehavior = initial |> changes

    let outputLagged :: SBehavior ((Sequence f g o, SEvent (NextComponent f g o)), VirtualElement Identity)
        outputLagged = lag outputBehavior


    -- That's a big type... Means we always have a latest event which will
    -- product later events.
    let fmappedNext :: SBehavior (SEvent (MomentIO ((Sequence f g o, SEvent (NextComponent f g o)), VirtualElement Identity)))
        fmappedNext = fmap (fmap runNextComponent . snd . fst) outputLagged

    let next :: SBehavior (SEvent ((Sequence f g o, SEvent (NextComponent f g o)), VirtualElement Identity))
        next = fmap (sequenceCommute (const (pure (Const ())))
                                     (fmap Identity . runIdentity)
                    )
                    fmappedNext

    let changes :: SEvent ((Sequence f g o, SEvent (NextComponent f g o)), VirtualElement Identity)
        changes = switch (flip const) next

    pure ((\((out, _), velem) -> (out, velem)) <$> outputBehavior)

nextComponentK
    :: forall f g o s t .
       Flow f g o s t
    -> (t -> NextComponent f g o)
    -> (s -> NextComponent f g o)
nextComponentK flow k = case flow of
    FlowComponent component -> \s -> FoundComponent component s k
    FlowArr f -> k . f
    FlowCompose left right ->
        let k' = nextComponentK left k
        in  nextComponentK right k'
    FlowFirst subFlow -> \(s, c) -> nextComponentK subFlow (\t -> k (t, c)) s
    FlowLeft subFlow -> \sum -> case sum of
        Left x -> nextComponentK subFlow (\t -> k (Left t)) x
        Right y -> k (Right y)

-- This is actually way cool. Look how using Void ties the knot given
-- in nextComponentK, which assumes the ability to produce a NextComponent.
nextComponent
    :: forall f g o s .
       Flow f g o s Void
    -> (s -> NextComponent f g o)
nextComponent flow = \s -> case flow of
    -- Note that this constructor is not ruled out by the Void.
    -- However, we know that its output event *can never fire*!
    -- So we can just throw absurd in as the continuation.
    -- Awesome :)
    FlowComponent component -> FoundComponent component s absurd
    FlowCompose left right ->
        let leftComponent = nextComponent left
        in  nextComponentK right leftComponent s

flowBehavior
    :: forall f g o s .
       Flow f g o s Void
    -> Kleisli MomentIO s (SBehavior (Sequence f g o, VirtualElement Identity))
flowBehavior flow = Kleisli $ \s -> runNextComponents (nextComponent flow s)

-- | Produce a virtual element which shows a complete flow, along with the
--   switched output.
runFlow
    :: forall f g o s .
       ( Switchable Identity f Identity g
       , SwitchesTo (SBehavior (Sequence f g o)) ~ Sequence f g o
       , Functor f
       , Functor g
       )
    => Flow f g o s Void
    -> Kleisli MomentIO s (Sequence f g o, VirtualElement Identity)
runFlow flow = Kleisli $ \s -> do
    output <- runKleisli (flowBehavior flow) s
    let outputs = fst <$> output
    let out = switch (flip const) outputs
    let velems = snd <$> output
    velem <- virtualElement (pure "div")
                            (pure (always mempty))
                            (pure (always mempty))
                            (pure (always mempty))
                            (pure (pure . node <$> velems))
    return (out, velem)
