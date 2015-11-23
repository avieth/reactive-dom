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
{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Sequence (

      Sequence(..)
    , (|>)
    , sequenceFirst
    , sequenceRest
    , sequenceOnce
    , sequenceBehavior
    , bundle
    , always
    , fromEvent
    , toEvent
    , toBehavior
    , sequenceUnion
    , sequenceUnion'
    , sequenceCommute
    , sequenceSwitch
    , sequenceReactimate
    , sequenceChoice

    {-
    , LiveSequence(..)
    , liveSequence
    , sequenceCurrent
    , sequenceNext
    -}

    , immediatelyAfter

    ) where

import Control.Monad.IO.Class
import Data.EitherBoth
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

data Sequence t = Sequence (MomentIO (t, Event t))

instance Functor Sequence where
    fmap f ~(Sequence m) = Sequence (fmap f' m)
      where
        f' ~(t, ev) = (f t, fmap f ev)

instance Applicative Sequence where
    pure = sequenceOnce
    mf <*> mx = (uncurry ($)) <$> (bundle mf mx)

x |> y = Sequence (pure (x, y))

sequenceFirst :: Sequence t -> MomentIO t
sequenceFirst ~(Sequence m) = fst <$> m

sequenceRest :: Sequence t -> MomentIO (Event t)
sequenceRest ~(Sequence m) = snd <$> m

sequenceOnce :: t -> Sequence t
sequenceOnce x = Sequence (pure (x, never))

sequenceBehavior :: Sequence t -> MomentIO (Behavior t)
sequenceBehavior ~(Sequence m) = do
    (t, ev) <- m
    stepper t ev

-- | This is the applicative <*> in another form.
bundle
    :: forall s t .
       Sequence s
    -> Sequence t
    -> Sequence (s, t)
bundle left right = Sequence $ do
    firstl :: s <- sequenceFirst left
    firstr :: t <- sequenceFirst right
    restl :: Event s <- sequenceRest left
    restr :: Event t <- sequenceRest right
    bl :: Behavior s <- stepper firstl restl
    br :: Behavior t <- stepper firstr restr
    -- Now we create an event which merges s and t, handling the case in which
    -- they fire simultaneously by using the EitherBoth type.
    let evLDiscriminated :: Event (EitherBoth s t)
        evLDiscriminated = OneLeft <$> restl
    let evRDiscriminated :: Event (EitherBoth s t)
        evRDiscriminated = OneRight <$> restr
    let evLR :: Event (EitherBoth s t)
        evLR = unionWith (\(OneLeft s) (OneRight t) -> Both s t)
                         (evLDiscriminated)
                         (evRDiscriminated)
    -- We throw in the behaviors for s and t, yielding an event which contains
    -- all the information we need in order to decide when to fire.
    let evLR' :: Event ((s, t), EitherBoth s t)
        evLR' = (,) <$> ((,) <$> bl <*> br) <@> evLR
    let pick :: ((s, t), EitherBoth s t) -> Maybe (s, t)
        pick x = case x of
            -- If both fire, just give them.
            (_, Both s t) -> Just (s, t)
            -- If only one fires, give the latest tuple.
            ((_, t), OneLeft s) -> Just (s, t)
            ((s, _), OneRight t) -> Just (s, t)
    return ((firstl, firstr), filterJust (pick <$> evLR'))

always :: t -> Sequence t
always x = x |> never

fromEvent :: Event t -> Sequence (Maybe t)
fromEvent event = Nothing |> (Just <$> event)

toEvent :: Sequence (Maybe t) -> MomentIO (Event t)
toEvent sequence = do
    x <- sequenceRest sequence
    return (filterJust x)

toBehavior :: Sequence t -> MomentIO (Behavior t)
toBehavior sequence = do
    first <- sequenceFirst sequence
    rest <- sequenceRest sequence
    stepper first rest

-- | Give an initial value and an event producing changes, and get a sequence
--   whose values are applications of those changes.
fromDeltas :: t -> Event (t -> t) -> Sequence t
fromDeltas x xs = Sequence $ mdo
    let event = (flip ($)) <$> behavior <@> xs
    behavior <- stepper x event
    return (x, event)

sequenceUnion :: (s -> s -> s) -> Sequence s -> Sequence s -> Sequence s
sequenceUnion unioner left right = Sequence $ do
    firstl <- sequenceFirst left
    firstr <- sequenceFirst right
    restl <- sequenceRest left
    restr <- sequenceRest right
    return (unioner firstl firstr, unionWith unioner restl restr)

sequenceChoice :: (Semigroup s) => Sequence s -> Sequence s -> Sequence s
sequenceChoice left right = Sequence $ do
    firstl <- sequenceFirst left
    firstr <- sequenceFirst right
    let first = firstl <> firstr
    restl <- sequenceRest left
    restr <- sequenceRest right
    let rest = unionWith (<>) restl restr
    return (first, rest)

instance Semigroup s => Semigroup (Sequence s) where
    (<>) = sequenceChoice

instance (Semigroup s, Monoid s) => Monoid (Sequence s) where
    mempty = always mempty
    mappend = (<>)

sequenceUnion' :: (s -> s -> s) -> Sequence s -> Event s -> Sequence s
sequenceUnion' unioner left right = Sequence $ do
    firstl <- sequenceFirst left
    restl <- sequenceRest left
    return (firstl, unionWith unioner restl right)

-- | Commute Sequence and MomentIO.
sequenceCommute :: Sequence (MomentIO t) -> MomentIO (Sequence t)
sequenceCommute sequence = do
    first :: MomentIO t <- sequenceFirst sequence
    rest :: Event (MomentIO t) <- sequenceRest sequence
    first' <- first
    rest' <- execute rest
    let mkSequence :: MomentIO (t, Event t)
        mkSequence = return (first', rest')
    return (Sequence mkSequence)

{-
sequenceExecute' :: Sequence (MomentIO t) -> Sequence t
sequenceExecute' sequence = Sequence $ do
    first :: MomentIO t <- sequenceFirst sequence
    rest :: Event (MomentIO t) <- sequenceRest sequence
    first' <- first
    rest' <- execute rest
    return (first', rest')

sequenceExecute'' :: Sequence (MomentIO t) -> Sequence t
sequenceExecute'' sequence = Sequence $ do
    first :: MomentIO t <- sequenceFirst sequence
    rest :: Event (MomentIO t) <- sequenceRest sequence
    first' <- first
    rest' <- execute rest
    rest'' <- immediatelyAfter rest'
    return (first', rest'')
-}

sequenceSwitch :: forall t . Sequence (Event t) -> MomentIO (Event t)
sequenceSwitch sequence = do
    first :: Event t <- sequenceFirst sequence
    rest :: Event (Event t) <- sequenceRest sequence
    let rests :: Event t
        rests = switchE rest
    return $ unionWith const rests first

sequenceReactimate :: Sequence (IO ()) -> MomentIO ()
sequenceReactimate sequence = do
    first <- sequenceFirst sequence
    rest <- sequenceRest sequence
    liftIO first
    reactimate rest
    return ()

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
{-
newtype LiveSequence t = LiveSequence (Behavior t, Event t)

instance Functor LiveSequence where
    fmap f (LiveSequence (b, e)) = LiveSequence (fmap f b, fmap f e)

liveSequence :: Sequence t -> MomentIO (LiveSequence t)
liveSequence ~(Sequence (t, ev)) = do
    b <- stepper t ev
    return (LiveSequence (b, ev))

{-
fromEvent :: t -> Event t -> MomentIO (LiveSequence t)
fromEvent first ev = do
    b <- stepper first ev
    return (LiveSequence (b, ev))
-}

sequenceCurrent :: LiveSequence t -> MomentIO t
sequenceCurrent (LiveSequence (b, _)) = valueB b

sequenceNext :: LiveSequence t -> Event t
sequenceNext (LiveSequence (_, e)) = e
-}

immediatelyAfter :: Event t -> MomentIO (Event t)
immediatelyAfter ev = do
    (ev', fire) <- newEvent
    reactimate (fire <$> ev)
    return ev'
