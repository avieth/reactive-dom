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
    , sequencePure
    , sequenceApply
    , bundle
    , always
    , fromChanges

    , LiveSequence(..)
    , fromSequence
    , fromEvent
    , sequenceCurrent
    , sequenceNext

    , immediatelyAfter

    ) where

import Control.Monad.IO.Class
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

sequencePure :: s -> Sequence s
sequencePure s = Sequence (s, never)

sequenceApply :: Sequence (s -> t) -> Sequence s -> MomentIO (Sequence t)
sequenceApply (Sequence (f, evf)) (Sequence (s, evs)) = do
    behf <- stepper f evf
    return (Sequence (f s, behf <@> evs))

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

-- Basically what we want here is a bundle of
--   1. An event to indicate all changes.
--   2. A behavior to indicate the previous value, at a given point when a
--      change occurs.
--   3. A behavior to indicate the latest value... so the old style should
--      suffice, no? If 
--
-- The crucial difference: the ability to ALWAYS get the VERY LATEST from
-- ANYWHERE, even if your action is indirectly induced by that very event! i.e.
-- you don't have the event's value from a reactimate or execute, because some
-- *other* reactimate or execute on that *same* event has called your action.
-- 
-- The fact that this situation can arise at all is a bit unsettling, though.
-- Perhaps a better solution is this: wait until the network has settled
-- before rendering the children.
newtype LiveSequence t = LiveSequence (Behavior t, Event t)

instance Functor LiveSequence where
    fmap f (LiveSequence (b, e)) = LiveSequence (fmap f b, fmap f e)

fromSequence :: Sequence t -> MomentIO (LiveSequence t)
fromSequence ~(Sequence (t, ev)) = do
    b <- stepper t ev
    return (LiveSequence (b, ev))

fromEvent :: t -> Event t -> MomentIO (LiveSequence t)
fromEvent first ev = do
    b <- stepper first ev
    return (LiveSequence (b, ev))

sequenceCurrent :: LiveSequence t -> MomentIO t
sequenceCurrent (LiveSequence (b, _)) = valueB b

sequenceNext :: LiveSequence t -> Event t
sequenceNext (LiveSequence (_, e)) = e

immediatelyAfter :: Event t -> MomentIO (Event t)
immediatelyAfter ev = do
    (ev', fire) <- newEvent
    reactimate (fire <$> ev)
    return ev'
