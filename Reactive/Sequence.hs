{-|
Module      : Reactive.Sequence
Description : Definition of Sequence.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE RecursiveDo #-}

module Reactive.Sequence (

      Sequence(..)
    , (|>)
    , sequenceFirst
    , sequenceRest
    , sequenceSingle
    , sequenceBehavior
    , bundle
    , always
    , fromChanges

    , LiveSequence
    , liveSequence
    , sequenceCurrent
    , sequenceCurrentLater
    , sequenceNext

    ) where

import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

newtype Sequence t = Sequence (t, Event t)

instance Functor Sequence where
    fmap f (Sequence (t, ev)) = Sequence (f t, fmap f ev)

x |> y = Sequence (x, y)

sequenceFirst :: Sequence t -> t
sequenceFirst (Sequence (t, _)) = t

sequenceRest :: Sequence t -> Event t
sequenceRest (Sequence (_, ev)) = ev

sequenceSingle :: t -> Sequence t
sequenceSingle x = Sequence (x, never)

sequenceBehavior :: Sequence t -> MomentIO (Behavior t)
sequenceBehavior (Sequence (t, ev)) = stepper t ev

-- | Create an event where the first component is the old, second is the new
--   value.
bundle :: Sequence t -> MomentIO (Event (t, t))
bundle sequence = do
    behavior <- stepper (sequenceFirst sequence) (sequenceRest sequence)
    return ((,) <$> behavior <@> sequenceRest sequence)

always :: t -> Sequence t
always x = x |> never

-- | Give an initial value and an event producing changes, and get a sequence
--   whose values are applications of those changes.
fromDeltas :: t -> Event (t -> t) -> MomentIO (Sequence t)
fromDeltas x xs = mdo
    let event = (flip ($)) <$> behavior <@> xs
    behavior <- stepper x event
    return (Sequence (x, event))

newtype LiveSequence t = LiveSequence (Behavior t, Event t)

instance Functor LiveSequence where
    fmap f (LiveSequence (b, e)) = LiveSequence (fmap f b, fmap f e)

liveSequence :: Sequence t -> MomentIO (LiveSequence t)
liveSequence ~(Sequence (t, ev)) = do
    b <- stepper t ev
    return (LiveSequence (b, ev))

sequenceCurrent :: LiveSequence t -> MomentIO t
sequenceCurrent (LiveSequence (b, _)) = valueB b

sequenceCurrentLater :: LiveSequence t -> MomentIO t
sequenceCurrentLater (LiveSequence (b, _)) = valueBLater b

sequenceNext :: LiveSequence t -> Event t
sequenceNext (LiveSequence (_, e)) = e
