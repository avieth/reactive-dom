{-|
Module      : 
Description : 
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
{-# LANGUAGE RecursiveDo #-}

module Reactive.DOM.Flow where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Category
import Control.Arrow
import Data.Void
import Data.Functor.Identity
import Data.Algebraic.Index
import Data.Algebraic.Sum
import Data.Algebraic.Product hiding (Component)
import Reactive.Banana.Frameworks (MomentIO)
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Component

-- An experiement in using category/arrow to describe a UI flow.
-- This gives us nice syntax, and as far as I can tell expresses everything
-- that a switched component can.

data Flow s t where
    FlowArr :: (s -> t) -> Flow s t
    FlowCompose :: Flow u t -> Flow s u -> Flow s t
    FlowFirst :: Flow s t -> Flow (s, c) (t, c)
    FlowLeft :: Flow s t -> Flow (Either s c) (Either t c)
    FlowComponent
        :: ( Component component
           , ComponentInput component ~ s
           , ComponentOutput component ~ SEvent t
           )
        => component
        -> Flow s t

type CompleteFlow s = Flow s Void

instance Category Flow where
    id = FlowArr id
    (.) = FlowCompose

instance Arrow Flow where
    arr = FlowArr
    first = FlowFirst

instance ArrowChoice Flow where
    left = FlowLeft

-- Here's what we need.
-- The idea is to produce the next component from a Flow.
-- When we flip t ~ Void we will find that we can always produce at least
-- one, since 3 of 5 constructors are ruled out.
--
-- Here we bundle all the information needed to run the component.
data NextComponent where
    FoundComponent
        :: ( Component component
           , ComponentInput component ~ s
           , ComponentOutput component ~ SEvent t
           )
        => component
        -> s
        -> (t -> NextComponent)
        -> NextComponent

runNextComponent
    :: NextComponent
    -> MomentIO (SEvent NextComponent, VirtualElement Identity)
runNextComponent (FoundComponent component input next) = do
    (output, velem) <- runComponent component input
    pure (next <$> output, velem)

runNextComponents :: NextComponent -> MomentIO (SBehavior (VirtualElement Identity))
runNextComponents component = mdo

    initial :: (SEvent NextComponent, VirtualElement Identity)
        <- runNextComponent component

    let outputBehavior :: SBehavior (SEvent NextComponent, VirtualElement Identity)
        outputBehavior = initial |> changes

    let outputLagged :: SBehavior (SEvent NextComponent, VirtualElement Identity)
        outputLagged = lag outputBehavior


    -- That's a big type... Means we always have a latest event which will
    -- product later events.
    let fmappedNext :: SBehavior (SEvent (MomentIO (SEvent NextComponent, VirtualElement Identity)))
        fmappedNext = fmap (fmap runNextComponent . fst) outputLagged

    let next :: SBehavior (SEvent (SEvent NextComponent, VirtualElement Identity))
        next = fmap (sequenceCommute (const (pure (Const ())))
                                     (fmap Identity . runIdentity)
                    )
                    fmappedNext

    let changes :: SEvent (SEvent NextComponent, VirtualElement Identity)
        changes = switch (flip const) next

    pure (snd <$> outputBehavior)

nextComponentK
    :: forall s t .
       Flow s t
    -> (t -> NextComponent)
    -> (s -> NextComponent)
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
    :: forall s .
       Flow s Void
    -> (s -> NextComponent)
nextComponent flow = \s -> case flow of
    -- Note that this constructor is not ruled out by the Void.
    -- However, we know that its output event *can never fire*!
    -- So we can just throw absurd in as the continuation.
    -- Awesome :)
    FlowComponent component -> FoundComponent component s absurd
    FlowCompose left right ->
        let leftComponent = nextComponent left
        in  nextComponentK right leftComponent s

runFlow
    :: forall s .
       Flow s Void
    -> Kleisli MomentIO s (SBehavior (VirtualElement Identity))
runFlow flow = Kleisli $ \s -> runNextComponents (nextComponent flow s)


