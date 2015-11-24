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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reactive.Sequence (

      Sequence(..)
    , (||>)
    , (|>)
    , always
    , sequenceFirst
    , sequenceRest
    , sequenceOnce
    , sequenceBehavior
    , bundle
    , fromEvent
    , toEvent
    , toBehavior
    , sequenceUnion
    , sequenceUnion'
    , sequenceCommute
    , sequenceCommute'
    , sequenceSwitch
    , sequenceSwitch'
    , sequenceReactimate
    , sequenceChoice

    , SequenceAlternative(..)

    {-
    , LiveSequence(..)
    , liveSequence
    , sequenceCurrent
    , sequenceNext
    -}

    , immediatelyAfter

    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.EitherBoth
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

import Debug.Trace

data Sequence t where
    Sequence :: MomentIO (s, MomentIO (Event s)) -> (s -> t) -> Sequence t

instance Functor Sequence where
    fmap f (Sequence m g) = Sequence m (fmap f g)

instance Applicative Sequence where
    pure = sequenceOnce
    mf <*> mx = (uncurry ($)) <$> (bundle mf mx)

(||>) :: MomentIO t -> MomentIO (Event t) -> Sequence t
x ||> y = Sequence ((\x -> (x, y)) <$> x) id

(|>) :: t -> Event t -> Sequence t
x |> y = Sequence (pure (x, pure y)) id

always :: t -> Sequence t
always x = x |> never

sequenceFirst :: Sequence t -> MomentIO t
sequenceFirst (Sequence m f) = (f . fst) <$> m

sequenceRest :: Sequence t -> MomentIO (Event t)
sequenceRest (Sequence m f) = do
    (_, rest) <- m
    ev <- rest
    return (f <$> ev)

sequenceOnce :: t -> Sequence t
sequenceOnce = always

sequenceBehavior :: Sequence t -> MomentIO (Behavior t)
sequenceBehavior (Sequence m f) = do
    (t, mkEv) <- m
    ev <- mkEv
    stepper (f t) (f <$> ev)

-- | This is the applicative <*> in another form.
bundle
    :: forall s t .
       Sequence s
    -> Sequence t
    -> Sequence (s, t)
bundle left right = Sequence content id
  where
    content = do
        firstl :: s <- sequenceFirst left
        firstr :: t <- sequenceFirst right
        let theRest = do
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
                return (filterJust (pick <$> evLR'))
        return ((firstl, firstr), theRest)

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
fromDeltas x xs = Sequence content id
  where
    content = mdo
        let event = (flip ($)) <$> behavior <@> xs
        behavior <- stepper x event
        return (x, return event)

sequenceUnion :: (s -> s -> s) -> Sequence s -> Sequence s -> Sequence s
sequenceUnion unioner left right = Sequence content id
  where
    content = do
        firstl <- sequenceFirst left
        firstr <- sequenceFirst right
        let theRest = do
                restl <- sequenceRest left
                restr <- sequenceRest right
                return (unionWith unioner restl restr)
        return (unioner firstl firstr, theRest)

sequenceChoice :: (Semigroup s) => Sequence s -> Sequence s -> Sequence s
sequenceChoice left right = (<>) <$> left <*> right
{-
sequenceChoice left right = Sequence content id
  where
    content = do
        firstl <- sequenceFirst left
        firstr <- sequenceFirst right
        let first = firstl <> firstr
        let theRest = do
                restl <- sequenceRest left
                restr <- sequenceRest right
                return (unionWith (<>) restl restr)
        return (first, theRest)
-}

instance Semigroup s => Semigroup (Sequence s) where
    (<>) = sequenceChoice

instance (Semigroup s, Monoid s) => Monoid (Sequence s) where
    mempty = always mempty
    mappend = (<>)

sequenceLatest
    :: forall s .
       ( )
    => (s -> s -> s)
    -> Sequence s
    -> Sequence s
    -> Sequence s
sequenceLatest disambiguator left right = Sequence content id
  where
    content :: MomentIO (s, MomentIO (Event s))
    content = do
        firstl :: s <- sequenceFirst left
        firstr :: s <- sequenceFirst right
        let first :: s
            first = disambiguator firstl firstr
        let rest :: MomentIO (Event s)
            rest = do restl <- sequenceRest left
                      restr <- sequenceRest right
                      pure (unionWith disambiguator restl restr)
        return (first, rest)

newtype SequenceAlternative s t = SequenceAlternative {
      runSequenceAlternative :: Sequence (s t)
    }

deriving instance Functor s => Functor (SequenceAlternative s)

instance Applicative s => Applicative (SequenceAlternative s) where
    pure = SequenceAlternative . pure . pure
    left <*> right = SequenceAlternative $
        (<*>) <$> (runSequenceAlternative left) <*> (runSequenceAlternative right)

instance Alternative s => Alternative (SequenceAlternative s) where
    empty = SequenceAlternative (always empty)
    left <|> right = SequenceAlternative $
        sequenceLatest (<|>)
                       (runSequenceAlternative left)
                       (runSequenceAlternative right)

sequenceUnion' :: (s -> s -> s) -> Sequence s -> Event s -> Sequence s
sequenceUnion' unioner left right = Sequence content id
  where
    content = do
        firstl <- sequenceFirst left
        restl <- sequenceRest left
        let theRest = return (unionWith unioner restl right)
        return (firstl, theRest)

-- | We're careful to ensure that each MomentIO is executed at most once, so
--   that derived sequences don't recompute these terms.
sequenceCommute :: Sequence (MomentIO t) -> MomentIO (Sequence t)
sequenceCommute sequence = do
    first :: MomentIO t <- sequenceFirst sequence
    first' <- first
    rest :: Event (MomentIO t) <- sequenceRest sequence
    ev :: Event t <- execute rest
    return (Sequence (pure (first', pure ev)) id)

sequenceCommute' :: forall t . Sequence (MomentIO t) -> Sequence t
sequenceCommute' sequence = Sequence content id
  where
    content :: MomentIO (t, MomentIO (Event t))
    content = do
        s :: Sequence t <- sequenceCommute sequence
        first <- sequenceFirst s
        return (first, sequenceRest s)

-- | This is strict. We clearly have to force both parts of the sequence
--   before we can come up with the switched event.
sequenceSwitch :: forall t . Sequence (Event t) -> MomentIO (Event t)
sequenceSwitch sequence = do
    -- We have to kindof jump through some hoops here, to ensure that
    -- we don't miss the first event. This is done by making two separate
    -- @Event t@s:
    --
    --   first :: Event t  from sequenceFirst sequence
    --   rest :: Event t   from switchE <$> sequenceRest sequence
    --
    -- and then keeping track of which one to let pass through, by making
    -- a Behavior Bool indicating whether the first event has been made
    -- obsolete by an occurrence of the second.
    e :: Event t <- sequenceFirst sequence
    es <- sequenceRest sequence
    esHasFired :: Behavior Bool <- stepper False (const True <$> es)
    let first :: Event t
        first = e
    let rest :: Event t
        rest = switchE es
    return $ unionWith const 
                       (filterApply ((const . not) <$> esHasFired) first)
                       (filterApply (const <$> esHasFired) rest)

-- | Like sequenceSwitch'' but we hide the MomentIO computation inside the
--   Sequence, and give Nothing for the initial value.
sequenceSwitch' :: forall t . Sequence (Event t) -> Sequence (Maybe t)
sequenceSwitch' sequence = Sequence content id
  where
    content :: MomentIO (Maybe t, MomentIO (Event (Maybe t)))
    content = return (Nothing, (fmap . fmap) Just (sequenceSwitch sequence))

sequenceReactimate :: Sequence (IO ()) -> MomentIO ()
sequenceReactimate sequence = do
    first <- sequenceFirst sequence
    rest <- sequenceRest sequence
    liftIO first
    reactimate rest
    return ()

immediatelyAfter :: Event t -> MomentIO (Event t)
immediatelyAfter ev = do
    (ev', fire) <- newEvent
    reactimate (fire <$> ev)
    return ev'
