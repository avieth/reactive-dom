{-|
Module      : Reactive.EventTransformer
Description : Definition of the EventTransformer category.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
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

newtype EventTransformer s t = EventTransformer {
      runEventTransformer :: Event s -> MomentIO (Event t)
    }

instance Profunctor EventTransformer where
    dimap l r et = EventTransformer $
        dimap (fmap l) ((fmap . fmap) r) (runEventTransformer et)

instance Category EventTransformer where
    id = EventTransformer return
    left . right = EventTransformer $ runEventTransformer left <=< runEventTransformer right

instance Arrow EventTransformer where
    arr f = EventTransformer $ \ev -> return (f <$> ev)
    -- To implement first, we rely on the assumption that, given
    --
    --   evsc :: Event (s, c)
    --
    -- the events
    --
    --   evs :: Event s
    --   evs = fst <$> evsc
    --
    --   evc :: Event c
    --   evc = snd <$> evc
    --
    -- will occur simultaneously.
    -- This allows us to, after much type juggling, use unionWith to bring the
    -- @c@ value back into the output @Event t@.
    --
    -- I think this is wrong. I think a proper implementation will hold the
    -- @c@ value as it is observed, and then just pair it with the @t@ once
    -- it's observed.
    --
    first (ar :: EventTransformer s t) = EventTransformer $ \(evsc :: Event (s, c)) -> do
        let evs :: Event s
            evs = fst <$> evsc
        let evc :: Event c
            evc = snd <$> evsc
        evt :: Event t <- runEventTransformer ar evs
        let evT :: Event (Maybe (Either (t, c) (Either t c)))
            evT = (Just . Right . Left) <$> evt
        let evC :: Event (Maybe (Either (t, c) (Either t c)))
            evC = (Just . Right . Right) <$> evc
        let unioner
                :: Maybe (Either (t, c) (Either t c))
                -> Maybe (Either (t, c) (Either t c))
                -> Maybe (Either (t, c) (Either t c))
            unioner x y = case (x, y) of
                (Just (Right (Left t)), Just (Right (Right c))) -> Just (Left (t, c))
                _ -> Nothing
        let unioned :: Event (Maybe (Either (t, c) (Either t c)))
            unioned = unionWith unioner evT evC
        let trimmer :: Maybe (Either (t, c) (Either t c)) -> Maybe (t, c)
            trimmer = (=<<) (either Just (const Nothing))
        return (filterJust (trimmer <$> unioned))

instance ArrowChoice EventTransformer where
    left (ar :: EventTransformer s t) = EventTransformer $ \(evsc :: Event (Either s c)) -> do
        let evs :: Event s
            evs = filterJust (either Just (const Nothing) <$> evsc)
        let evc :: Event c
            evc = filterJust (either (const Nothing) Just <$> evsc)
        evt :: Event t <- runEventTransformer ar evs
        let evT :: Event (Either t c)
            evT = Left <$> evt
        let evC :: Event (Either t c)
            evC = Right <$> evc
        let unioned :: Event (Either t c)
            unioned = unionWith const evT evC
        return unioned

-- | Run two event transformers in parallel. The output events may happen
--   simultaneously.
parallel
    :: forall s1 s2 t1 t2 .
       EventTransformer s1 t1
    -> EventTransformer s2 t2
    -> EventTransformer (s1, s2) (EitherBoth t1 t2)
parallel one two = EventTransformer $ \ev -> do
    evOne :: Event t1 <- runEventTransformer one (fst <$> ev)
    evTwo :: Event t2 <- runEventTransformer two (snd <$> ev)
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
await :: forall t1 t2 . EventTransformer (EitherBoth t1 t2) (t1, t2)
await = EventTransformer $ \ev -> do

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

{-
-- We need to feed c back into itself.
--
--     EventTransformer (s, c) (t, c)
--
--     ______________________________
--     EventTransformer s t
--
instance ArrowLoop EventTransformer where
    loop (ar :: EventTransformer (s, c) (t, c)) = EventTransformer $ \evs -> mdo

        -- How to construct an evsc? The principal question: when exactly does
        -- the event fire?? Presumably that's built-in to @ar@. Yeah, it
        -- determines precisely when the output (t, c) will fire. And when
        -- that fires, just pipe it back to the input. BUT we have to wait for
        -- that @s@ value!
        --
        -- Operationally: we obtain an @s@ from @evs@. Then from where do we
        -- pull the @c@?? We just can't make one. This is impossible.
        evsc :: Event (s, c) <- evtc
        evtc :: Event (t, c) <- runEventTransformer ar evsc

        return (fst <$> evtc)
-}
