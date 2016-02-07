{-|
Module      : Reactive.EventTransformer
Description : Definition of the EventTransformer category.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

DEPRECATED
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Reactive.EventTransformer where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Control.Monad
import Data.Profunctor
import Data.EitherBoth
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Eventful
import Reactive.Sequence

type EventTransformer s t = Kleisli Eventful (Event s) (Event t)

eventTransformerNever :: EventTransformer s t
eventTransformerNever = Kleisli $ \_ -> return never

runEventTransformer :: EventTransformer s t -> Event s -> Eventful (Event t)
runEventTransformer = runKleisli

{-
-- | When the input event fires, we fire an event with the behavior sampled
--   at the time of the input event firing.
sampleBehavior :: Behavior t -> EventTransformer s t
sampleBehavior b = Kleisli $ \ev -> do
    (ev', fire) <- newEvent
    reactimate (fire <$> (b <@ ev))
    return ev' 

factorEventTransformer
    :: (t -> Either l r)
    -> EventTransformer s t
    -> MomentIO (Event l, EventTransformer s r)
factorEventTransformer f et = do
    (ev', fire) <- newEvent
    let et' = Kleisli $ \ev -> do
                  ev' <- runKleisli et ev
                  let passThrough = filterJust ((either (const Nothing) Just . f) <$> ev')
                  let usurp = filterJust ((either Just (const Nothing) . f) <$> ev')
                  reactimate (fire <$> usurp)
                  return passThrough
    return (ev', et')

-- | Run two event transformers in parallel. The output events may happen
--   simultaneously.
scatter
    :: forall s1 s2 t1 t2 .
       EventTransformer s1 t1
    -> EventTransformer s2 t2
    -> EventTransformer (s1, s2) (EitherBoth t1 t2)
scatter one two = Kleisli $ \ev -> do
    evOne :: Event t1 <- runKleisli one (fst <$> ev)
    evTwo :: Event t2 <- runKleisli two (snd <$> ev)
    let evOne' :: Event (EitherBoth t1 t2)
        evOne' = OneLeft <$> evOne
    let evTwo' :: Event (EitherBoth t1 t2)
        evTwo' = OneRight <$> evTwo
    let unioner :: EitherBoth t1 t2 -> EitherBoth t1 t2 -> EitherBoth t1 t2
        unioner left right = case (left, right) of
            (OneLeft x, OneRight y) -> Both x y
            -- Other cases are impossible in the usage here.
            -- See definition of evOne' evTwo'
    return (unionWith unioner evOne' evTwo')

-- | Emit an event as soon as both summands have fired *at least once*. If
--   one summand fires more than once before the other's first firing, then
--   the latest value is given in the output.
--   Think of it as kinda sorta inverse to scatter.
synchronize :: forall t1 t2 . EventTransformer (EitherBoth t1 t2) (t1, t2)
synchronize = Kleisli $ \ev -> do

    let pickEvOne :: EitherBoth t1 t2 -> Maybe t1
        pickEvOne e = case e of
            OneLeft x -> Just x
            _ -> Nothing

    let pickEvTwo :: EitherBoth t1 t2 -> Maybe t2
        pickEvTwo e = case e of
            OneRight x -> Just x
            _ -> Nothing

    let evOne :: Event t1
        evOne = filterJust (pickEvOne <$> ev)

    let evTwo :: Event t2
        evTwo = filterJust (pickEvTwo <$> ev)
    
    -- If they fire at the same time, return the tuple; otherwise, create
    -- a behavior using the event value that fired, and pair it up with the
    -- first occurrence of the second event.
    let executor :: EitherBoth t1 t2 -> MomentIO (Either (t1, t2) (Event (t1, t2)))
        executor e = case e of
            Both x y -> return (Left (x, y))
            OneLeft x -> do b <- stepper x evOne
                            return (Right ((,) <$> b <@> evTwo))
            OneRight y -> do b <- stepper y evTwo
                             return (Right ((flip (,)) <$> b <@> evOne))

    ev' :: Event (Either (t1, t2) (Event (t1, t2))) <- execute (executor <$> ev)

    let factorLeft :: Event (t1, t2)
        factorLeft = filterJust ((either Just (const Nothing)) <$> ev')

    let factorRight :: Event (Event (t1, t2))
        factorRight = filterJust ((either (const Nothing) Just) <$> ev')

    return (unionWith const factorLeft (switchE factorRight))

-- | This event fires whenever either of the input events fires, and always
--   gives the latest values.
--eventFold :: Event a -> Event b -> Event (a, b)
--eventFold eva evb = 
-}
