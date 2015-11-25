{-|
Module      : Reactive.Sequence
Description : Definition of Sequence, which generalizes and unifies the
              Reactive.Banana Event and Behavior types.
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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

module Reactive.Sequence where
{-
      Sequence(..)
    , SValue
    , SEvent
    , SBehavior
    , sEventToSBehavior
    , sBehaviorToSEvent
    , eventToSEvent
    , sEventToEvent
    , sBehaviorToBehavior
    , (|>)
    , always
    , sequenceFirst
    , sequenceRest
    , sequenceBehavior
    , bundle
    , fromEvent
    , toEvent
    , toBehavior
    , sequenceUnion
    , sequenceCommute
    , sequenceCommute'
    , sequenceSwitch
    , sequenceSwitch'
    , switchSequence
    , sSwitchE
    , sSwitchE'
    , sequenceReactimate
    , immediatelyAfter
-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Void
import Data.Proxy
import Data.Functor.Identity
import Data.Functor.Constant
import Data.EitherBoth
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

-- | A @Sequence@ is like an @Event@, and also like a @Behavior@. It's
--   one functorial value, followed by an @Event@ bringing more functorial
--   values through a possibly different functor. By modulating these
--   parameters we uncover types which resemble @Event@ and @Behavior@:
--
--     @
--       Sequence (Const ()) Identity ~ Event
--       Sequence Identity Identity ~ Behavior
--     @
--
data Sequence (f :: * -> *) (g :: * -> *) (t :: *) where
    Sequence :: MomentIO (f s, MomentIO (Event (g s))) -> (s -> t) -> Sequence f g t

-- | A @Sequence@ with no values coming from an @Event@.
type SValue = Sequence Identity (Const Void)

-- | A @Sequence@ whose initial value contains no information.
--   This is much like an @Event@.
type SEvent = Sequence (Const ()) Identity

-- | A @Sequence@ with an initial value. This is much like a @Behavior@; it
--   contains enough information to produce a @Behavior@.
type SBehavior = Sequence Identity Identity

anyToConst :: forall s t . s -> Const () t
anyToConst _ = Const ()

sEventToSBehavior :: t -> SEvent t -> SBehavior t
sEventToSBehavior t sevent = Sequence (pure (Identity t, sequenceRest sevent)) id

sBehaviorToSEvent :: SBehavior t -> SEvent t
sBehaviorToSEvent sbehavior = Sequence (pure (Const (), sequenceRest sbehavior)) id

eventToSEvent :: Event t -> SEvent t
eventToSEvent ev = Sequence (pure (Const (), pure (Identity <$> ev))) id

sEventToEvent :: SEvent t -> MomentIO (Event t)
sEventToEvent sevent = (fmap . fmap) runIdentity (sequenceRest sevent)

sBehaviorToBehavior :: SBehavior t -> MomentIO (Behavior t)
sBehaviorToBehavior sbehavior = do
    initial :: Identity t <- sequenceFirst sbehavior
    rest :: Event (Identity t) <- sequenceRest sbehavior
    b <- stepper initial rest
    return (runIdentity <$> b)

instance Functor (Sequence f g) where
    fmap f (Sequence m g) = Sequence m (fmap f g)

-- | A more general applicative-style interface is given by
--
--     @
--       always :: f t -> Sequence f g t
--       (<%>) :: Sequence f1 g1 (s -> t) -> Sequence f2 g2 s -> Sequence f3 g3 t
--     @
--
--   Note that for (<%>) we rely on the Bundleable type class to determine
--   what the output parameters @f3@ and @g3@ shall be. This allows us to
--   combine SEvent and SBehavior, for example, to get an SEvent.
instance
    ( Applicative f
    , Applicative (BundleableIntermediate f f f f)
    , Bundleable f f f f
    , BundleableOutputF f f f f ~ f
    , BundleableOutputG f f f f ~ f
    ) => Applicative (Sequence f f)
  where
    pure = always . pure
    (<*>) = (<%>)

{-
instance (Semigroup s, Applicative (Sequence f f)) => Semigroup (Sequence f f s) where
    (<>) = sequenceChoice

instance (Semigroup s, Monoid s, Applicative (Sequence f f)) => Monoid (Sequence f f s) where
    mempty = always (pure mempty)
    mappend = (<>)
-}

(|>) :: f t -> Event (g t) -> Sequence f g t
x |> y = Sequence (pure (x, pure y)) id

always :: f t -> Sequence f g t
always x = x |> never

sequenceFirst :: Functor f => Sequence f g t -> MomentIO (f t)
sequenceFirst (Sequence m f) = (fmap f . fst) <$> m

sequenceRest :: Functor g => Sequence f g t -> MomentIO (Event (g t))
sequenceRest (Sequence m f) = do
    (_, rest) <- m
    ev <- rest
    return ((fmap . fmap) f ev)

sequenceBehavior :: Functor f => Sequence f f t -> MomentIO (Behavior (f t))
sequenceBehavior (Sequence m f) = do
    (t, mkEv) <- m
    ev <- mkEv
    stepper (f <$> t) ((fmap . fmap) f ev)

sequenceTrans
    :: forall f1 f2 g1 g2 t .
       (forall a . f1 a -> f2 a)
    -> (forall a . g1 a -> g2 a)
    -> Sequence f1 g1 t
    -> Sequence f2 g2 t
sequenceTrans transF transG (Sequence content h) = Sequence (alter <$> content) h
  where
    alter :: forall s . (f1 s, MomentIO (Event (g1 s))) -> (f2 s, MomentIO (Event (g2 s)))
    alter (s, rest) = (transF s, (fmap . fmap) transG rest)

-- | This is the applicative <*> in another form.
--
--   But we want to be able to have f /= g. As an example, combining an
--   SEvent and an SBehavior should give an SEvent: it will fire as soon as
--   the SEvent fires, using the most recent value from the SBehavior.
--   Combining SEvent and SEvent is simple, same for SBehavior and SBehavior.
--   But this means we just can't have an applicative, for an applicative
--   instance demands the terms have the same type as the output. 
bundle
    :: forall f s t .
       ( Applicative f )
    => Sequence f f s
    -> Sequence f f t
    -> Sequence f f (s, t)
bundle left right = Sequence content id
  where
    content :: MomentIO (f (s, t), MomentIO (Event (f (s, t))))
    content = do
        firstl :: f s <- sequenceFirst left
        firstr :: f t <- sequenceFirst right
        let theRest :: MomentIO (Event (f (s, t)))
            theRest = do
                restl :: Event (f s) <- sequenceRest left
                restr :: Event (f t) <- sequenceRest right
                bl :: Behavior (f s) <- stepper firstl restl
                br :: Behavior (f t) <- stepper firstr restr
                -- Now we create an event which merges s and t, handling the case in which
                -- they fire simultaneously by using the EitherBoth type.
                let evLDiscriminated :: Event (EitherBoth (f s) (f t))
                    evLDiscriminated = OneLeft <$> restl
                let evRDiscriminated :: Event (EitherBoth (f s) (f t))
                    evRDiscriminated = OneRight <$> restr
                let evLR :: Event (EitherBoth (f s) (f t))
                    evLR = unionWith (\(OneLeft s) (OneRight t) -> Both s t)
                                     (evLDiscriminated)
                                     (evRDiscriminated)
                -- We throw in the behaviors for s and t, yielding an event which contains
                -- all the information we need in order to decide when to fire.
                let evLR' :: Event ((f s, f t), EitherBoth (f s) (f t))
                    evLR' = (,) <$> ((,) <$> bl <*> br) <@> evLR
                let pick :: ((f s, f t), EitherBoth (f s) (f t)) -> Maybe (f (s, t))
                    pick x = case x of
                        -- If both fire, just give them.
                        (_, Both s t) -> Just ((,) <$> s <*> t)
                        -- If only one fires, give the latest tuple.
                        ((_, t), OneLeft s) -> Just ((,) <$> s <*> t)
                        ((s, _), OneRight t) -> Just ((,) <$> s <*> t)
                return (filterJust (pick <$> evLR'))
        return ((,) <$> firstl <*> firstr, theRest)

-- | A more general form of @bundle@, in which the @Sequence@s can have
--   disparate functor parameters. With this form, we can bundle an
--   SEvent with an SBehavior to obtain an SEvent, by choosing h = Maybe
--    
--      @
--        magic :: SEvent s -> SBehavior t -> SEvent (s, t)
--        magic =  bundle' (id)
--                         (const (Const ()))
--                         (id)
--                         (id)
--                         (const Nothing)
--                         (Just . runIdentity)
--                         (maybe (Left (Const ())) (Right . Identity))
--      @
--
bundle'
    :: forall f1 f2 f3 g1 g2 g3 h s t .
       ( Applicative f1
       , Applicative f2
       , Applicative f3
       , Applicative g1
       , Applicative g2
       , Applicative g3
       , Applicative h
       )
    => (forall a . f1 a -> f3 a)
    -> (forall a . f2 a -> f3 a)
    -> (forall a . g1 a -> g3 a)
    -> (forall a . g2 a -> g3 a)
    -> (forall a . f1 a -> h a)
    -> (forall a . f2 a -> h a)
    -> (forall a . g1 a -> h a)
    -> (forall a . g2 a -> h a)
    -> (forall a . h a -> Either (f3 a) (g3 a))
    -> Sequence f1 g1 s
    -> Sequence f2 g2 t
    -> Sequence f3 g3 (s, t)
bundle' transF1 transF2 transG1 transG2 transF1H transF2H transG1H transG2H outH left right = Sequence content id
  where
    content :: MomentIO (f3 (s, t), MomentIO (Event (g3 (s, t))))
    content = do 
       firstl :: f1 s <- sequenceFirst left
       firstr :: f2 t <- sequenceFirst right
       let theRest :: MomentIO (Event (g3 (s, t)))
           theRest = do
               restl :: Event (g1 s) <- sequenceRest left
               restr :: Event (g2 t) <- sequenceRest right
               bl :: Behavior (h s) <- stepper (transF1H firstl) (transG1H <$> restl)
               br :: Behavior (h t) <- stepper (transF2H firstr) (transG2H <$> restr)
               -- Now we create an event which merges s and t, handling the case in which
               -- they fire simultaneously by using the EitherBoth type.
               let evLDiscriminated :: Event (EitherBoth (g1 s) (g2 t))
                   evLDiscriminated = OneLeft <$> restl
               let evRDiscriminated :: Event (EitherBoth (g1 s) (g2 t))
                   evRDiscriminated = OneRight <$> restr
               let evLR :: Event (EitherBoth (g1 s) (g2 t))
                   evLR = unionWith (\(OneLeft s) (OneRight t) -> Both s t)
                                    (evLDiscriminated)
                                    (evRDiscriminated)
               -- We throw in the behaviors for s and t, yielding an event which contains
               -- all the information we need in order to decide when to fire.
               let evLR' :: Event ((h s, h t), EitherBoth (g1 s) (g2 t))
                   evLR' = (,) <$> ((,) <$> bl <*> br) <@> evLR
               let pick :: ((h s, h t), EitherBoth (g1 s) (g2 t)) -> Maybe (g3 (s, t))
                   pick x = case x of
                       -- If both fire, just give them.
                       (_, Both s t) -> Just ((,) <$> transG1 s <*> transG2 t)
                       -- If only one fires, give the latest tuple, but only
                       -- if the behavior yields a g3.
                       ((_, t), OneLeft s) -> case outH t of
                           Left _ -> Nothing
                           Right t' -> Just ((,) <$> transG1 s <*> t')
                       ((s, _), OneRight t) -> case outH s of
                           Left _ -> Nothing
                           Right s' -> Just ((,) <$> s' <*> transG2 t)
               return (filterJust (pick <$> evLR'))

       return ((,) <$> transF1 firstl <*> transF2 firstr, theRest)

class Bundleable (f1 :: * -> *) (f2 :: * -> *) (g1 :: * -> *) (g2 :: * -> *) where
    type BundleableOutputF f1 f2 g1 g2 :: * -> *
    type BundleableOutputG f1 f2 g1 g2 :: * -> *
    type BundleableIntermediate f1 f2 g1 g2 :: * -> *
    bundleTransF1
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . f1 a -> BundleableOutputF f1 f2 g1 g2 a)
    bundleTransF2
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . f2 a -> BundleableOutputF f1 f2 g1 g2 a)
    bundleTransG1
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . g1 a -> BundleableOutputG f1 f2 g1 g2 a)
    bundleTransG2
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . g2 a -> BundleableOutputG f1 f2 g1 g2 a)
    bundleTransIntermediateF1
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . f1 a -> BundleableIntermediate f1 f2 g1 g2 a)
    bundleTransIntermediateF2
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . f2 a -> BundleableIntermediate f1 f2 g1 g2 a)
    bundleTransIntermediateG1
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . g1 a -> BundleableIntermediate f1 f2 g1 g2 a)
    bundleTransIntermediateG2
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . g2 a -> BundleableIntermediate f1 f2 g1 g2 a)
    bundleIntermediateOut
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . BundleableIntermediate f1 f2 g1 g2 a -> Either (BundleableOutputF f1 f2 g1 g2 a) (BundleableOutputG f1 f2 g1 g2 a))

-- | Proof that we can bundle SEvent and SBehavior.
instance Bundleable (Const ()) Identity Identity Identity where
    type BundleableOutputF (Const ()) Identity Identity Identity = Const ()
    type BundleableOutputG (Const ()) Identity Identity Identity = Identity
    type BundleableIntermediate (Const ()) Identity Identity Identity = Maybe
    bundleTransF1 _ = const (Const ())
    bundleTransF2 _ = const (Const ())
    bundleTransG1 _ = id
    bundleTransG2 _ = id
    bundleTransIntermediateF1 _ = const Nothing
    bundleTransIntermediateF2 _ = Just . runIdentity
    bundleTransIntermediateG1 _ = Just . runIdentity
    bundleTransIntermediateG2 _ = Just . runIdentity
    bundleIntermediateOut _ = maybe (Left (Const ())) (Right . Identity)

-- | Proof that we can bundle SBehavior and SEvent.
instance Bundleable Identity (Const ()) Identity Identity where
    type BundleableOutputF Identity (Const ()) Identity Identity = Const ()
    type BundleableOutputG Identity (Const ()) Identity Identity = Identity
    type BundleableIntermediate Identity (Const ()) Identity Identity = Maybe
    bundleTransF1 _ = const (Const ())
    bundleTransF2 _ = const (Const ())
    bundleTransG1 _ = id
    bundleTransG2 _ = id
    bundleTransIntermediateF1 _ = Just . runIdentity
    bundleTransIntermediateF2 _ = const Nothing
    bundleTransIntermediateG1 _ = Just . runIdentity
    bundleTransIntermediateG2 _ = Just . runIdentity
    bundleIntermediateOut _ = maybe (Left (Const ())) (Right . Identity)

-- | Proof that we can bundle SEvent and SEvent.
instance Bundleable (Const ()) (Const ()) Identity Identity where
    type BundleableOutputF (Const ()) (Const ()) Identity Identity = Const ()
    type BundleableOutputG (Const ()) (Const ()) Identity Identity = Identity
    type BundleableIntermediate (Const ()) (Const ()) Identity Identity = Maybe
    bundleTransF1 _ = const (Const ())
    bundleTransF2 _ = const (Const ())
    bundleTransG1 _ = id
    bundleTransG2 _ = id
    bundleTransIntermediateF1 _ = const Nothing
    bundleTransIntermediateF2 _ = const Nothing
    bundleTransIntermediateG1 _ = Just . runIdentity
    bundleTransIntermediateG2 _ = Just . runIdentity
    bundleIntermediateOut _ = maybe (Left (Const ())) (Right . Identity)

-- | Proof that we can bundle SBehavior and SBehavior.
instance Bundleable Identity Identity Identity Identity where
    type BundleableOutputF Identity Identity Identity Identity = Identity
    type BundleableOutputG Identity Identity Identity Identity = Identity
    type BundleableIntermediate Identity Identity Identity Identity = Identity
    bundleTransF1 _ = id
    bundleTransF2 _ = id
    bundleTransG1 _ = id
    bundleTransG2 _ = id
    bundleTransIntermediateF1 _ = id
    bundleTransIntermediateF2 _ = id
    bundleTransIntermediateG1 _ = id
    bundleTransIntermediateG2 _ = id
    -- We choose Right to indicate that it's available.
    -- Has we chosen Left, the bundled behavior would never update.
    bundleIntermediateOut _ = Right

bundle''
    :: forall f1 f2 g1 g2 s t .
       ( Applicative f1
       , Applicative f2
       , Applicative (BundleableOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (BundleableOutputG f1 f2 g1 g2)
       , Applicative (BundleableIntermediate f1 f2 g1 g2)
       , Bundleable f1 f2 g1 g2
       )
    => Sequence f1 g1 s
    -> Sequence f2 g2 t
    -> Sequence (BundleableOutputF f1 f2 g1 g2) (BundleableOutputG f1 f2 g1 g2) (s, t)
bundle'' = bundle' (bundleTransF1 proxy)
                   (bundleTransF2 proxy)
                   (bundleTransG1 proxy)
                   (bundleTransG2 proxy)
                   (bundleTransIntermediateF1 proxy)
                   (bundleTransIntermediateF2 proxy)
                   (bundleTransIntermediateG1 proxy)
                   (bundleTransIntermediateG2 proxy)
                   (bundleIntermediateOut proxy)
  where
    proxy :: Proxy '(f1, f2, g1, g2)
    proxy = Proxy

infixl 4 <%>
(<%>)
    :: forall f1 f2 g1 g2 s t .
       ( Applicative f1
       , Applicative f2
       , Applicative (BundleableOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (BundleableOutputG f1 f2 g1 g2)
       , Applicative (BundleableIntermediate f1 f2 g1 g2)
       , Bundleable f1 f2 g1 g2
       )
    => Sequence f1 g1 (s -> t)
    -> Sequence f2 g2 s
    -> Sequence (BundleableOutputF f1 f2 g1 g2) (BundleableOutputG f1 f2 g1 g2) t
left <%> right = (uncurry ($)) <$> (bundle'' left right)

infixl 4 <⌚>
-- | An operator that looks kindof like <*>, but with a watch instead of
--   a *.
(<⌚>)
    :: forall f1 f2 g1 g2 s t .
       ( Applicative f1
       , Applicative f2
       , Applicative (BundleableOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (BundleableOutputG f1 f2 g1 g2)
       , Applicative (BundleableIntermediate f1 f2 g1 g2)
       , Bundleable f1 f2 g1 g2
       )
    => Sequence f1 g1 (s -> t)
    -> Sequence f2 g2 s
    -> Sequence (BundleableOutputF f1 f2 g1 g2) (BundleableOutputG f1 f2 g1 g2) t
left <⌚> right = (uncurry ($)) <$> (bundle'' left right)

-- | Sort of like @<|>@, as @<%>@ is for @<*>@. This combinator gives the
--   @Sequence@ of the *latest* values for both input @Sequence@s, subject
--   to disambiguator functions for simultaneous occurrences.
--   Contrast with @<%>@, which downgrades input @Sequence@s to their
--   greatest lower bound; this one upgrades them to the least upper bound!
--   This is intuitive: a behavior always has a latest value, so if we union
--   it with an event, which may not have a latest value, we still have a
--   latest value.
sequenceUnion
    :: forall f1 f2 f3 g1 g2 g3 s .
       ( Applicative f1
       , Applicative f2
       , Applicative f3
       , Applicative g1
       , Applicative g2
       , Applicative g3
       )
    => (s -> s -> s)
    -> ((s -> s -> s) -> f1 s -> f2 s -> f3 s)
    -> ((s -> s -> s) -> g3 s -> g3 s -> g3 s)
    -> (forall a . g1 a -> g3 a)
    -> (forall a . g2 a -> g3 a)
    -> Sequence f1 g1 s
    -> Sequence f2 g2 s
    -> Sequence f3 g3 s
sequenceUnion disambiguator disambiguatorF disambiguatorG transG1 transG2 left right = Sequence content id
  where
    content = do
        firstl :: f1 s <- sequenceFirst left
        firstr :: f2 s <- sequenceFirst right
        let theRest = do
                restl :: Event (g1 s) <- sequenceRest left
                restr :: Event (g2 s) <- sequenceRest right
                return (unionWith (disambiguatorG disambiguator) (transG1 <$> restl) (transG2 <$> restr))
        return (disambiguatorF disambiguator firstl firstr, theRest)

class Unionable f1 f2 g1 g2 where
    type UnionOutputF f1 f2 g1 g2 :: * -> *
    type UnionOutputG f1 f2 g1 g2 :: * -> *
    unionDisambiguatorF
        :: Proxy '(f1, f2, g1, g2)
        -> (s -> s -> s)
        -> f1 s
        -> f2 s
        -> UnionOutputF f1 f2 g1 g2 s
    unionDisambiguatorG
        :: Proxy '(f1, f2, g1, g2)
        -> (s -> s -> s)
        -> UnionOutputG f1 f2 g1 g2 s
        -> UnionOutputG f1 f2 g1 g2 s
        -> UnionOutputG f1 f2 g1 g2 s
    unionTransG1
        :: Proxy '(f1, f2, g1, g2)
        -> (forall s . g1 s -> UnionOutputG f1 f2 g1 g2 s)
    unionTransG2
        :: Proxy '(f1, f2, g1, g2)
        -> (forall s . g2 s -> UnionOutputG f1 f2 g1 g2 s)

instance Unionable (Const ()) Identity Identity Identity where
    type UnionOutputF (Const ()) Identity Identity Identity = Identity
    type UnionOutputG (Const ()) Identity Identity Identity = Identity
    unionDisambiguatorF _ _ _ = id
    unionDisambiguatorG _ f l r = f <$> l <*> r
    unionTransG1 _ = id
    unionTransG2 _ = id

instance Unionable Identity (Const ()) Identity Identity where
    type UnionOutputF Identity (Const ()) Identity Identity = Identity
    type UnionOutputG Identity (Const ()) Identity Identity = Identity
    unionDisambiguatorF _ _ x = const x
    unionDisambiguatorG _ f l r = f <$> l <*> r
    unionTransG1 _ = id
    unionTransG2 _ = id

instance Unionable Identity Identity Identity Identity where
    type UnionOutputF Identity Identity Identity Identity = Identity
    type UnionOutputG Identity Identity Identity Identity = Identity
    unionDisambiguatorF _ f l r = f <$> l <*> r
    unionDisambiguatorG _ f l r = f <$> l <*> r
    unionTransG1 _ = id
    unionTransG2 _ = id

instance Unionable (Const ()) (Const ()) Identity Identity where
    type UnionOutputF (Const ()) (Const ()) Identity Identity = Const ()
    type UnionOutputG (Const ()) (Const ()) Identity Identity = Identity
    unionDisambiguatorF _ f _ _ = Const ()
    unionDisambiguatorG _ f l r = f <$> l <*> r
    unionTransG1 _ = id
    unionTransG2 _ = id

sequenceUnion'
    :: forall f1 f2 g1 g2 s .
       ( Unionable f1 f2 g1 g2
       , Applicative f1
       , Applicative f2
       , Applicative (UnionOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (UnionOutputG f1 f2 g1 g2)
       )
    => (s -> s -> s)
    -> Sequence f1 g1 s
    -> Sequence f2 g2 s
    -> Sequence (UnionOutputF f1 f2 g1 g2) (UnionOutputG f1 f2 g1 g2) s
sequenceUnion' disambiguator = sequenceUnion disambiguator
                                             (unionDisambiguatorF proxy)
                                             (unionDisambiguatorG proxy)
                                             (unionTransG1 proxy)
                                             (unionTransG2 proxy)
  where
    proxy :: Proxy '(f1, f2, g1, g2)
    proxy = Proxy

infixl 4 <||>
(<||>)
    :: forall f1 f2 g1 g2 s .
       ( Unionable f1 f2 g1 g2
       , Applicative f1
       , Applicative f2
       , Applicative (UnionOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (UnionOutputG f1 f2 g1 g2)
       , Semigroup s
       )
    => Sequence f1 g1 s
    -> Sequence f2 g2 s
    -> Sequence (UnionOutputF f1 f2 g1 g2) (UnionOutputG f1 f2 g1 g2) s
(<||>) = sequenceUnion' (<>)

fromEvent :: Event t -> Sequence (Const ()) Identity t
fromEvent event = (Const ()) |> (Identity <$> event)

toEvent :: Sequence (Const ()) Identity t -> MomentIO (Event t)
toEvent sequence = do
    x <- sequenceRest sequence
    return (runIdentity <$> x)

toBehavior :: Functor f => Sequence f f t -> MomentIO (Behavior (f t))
toBehavior sequence = do
    first <- sequenceFirst sequence
    rest <- sequenceRest sequence
    stepper first rest

-- | We're careful to ensure that each MomentIO is executed at most once, so
--   that derived sequences don't recompute these terms.
sequenceCommute
    :: forall f g t .
       ( Functor f, Functor g )
    => (forall s . f (MomentIO s) -> MomentIO (f s))
    -> (forall s . g (MomentIO s) -> MomentIO (g s))
    -> Sequence f g (MomentIO t)
    -> MomentIO (Sequence f g t)
sequenceCommute commuteF commuteG sequence = do
    first :: f (MomentIO t) <- sequenceFirst sequence
    first' :: f t <- commuteF first
    rest :: Event (g (MomentIO t)) <- sequenceRest sequence
    ev :: Event (g t) <- execute (commuteG <$> rest)
    return (Sequence (pure (first', pure ev)) id)

sequenceCommute'
    :: forall f g t .
       ( Functor f, Functor g )
    => (forall s . f (MomentIO s) -> MomentIO (f s))
    -> (forall s . g (MomentIO s) -> MomentIO (g s))
    -> Sequence f g (MomentIO t)
    -> Sequence f g t
sequenceCommute' commuteF commuteG sequence = Sequence content id
  where
    content :: MomentIO (f t, MomentIO (Event (g t)))
    content = do
        s :: Sequence f g t <- sequenceCommute commuteF commuteG sequence
        first :: f t <- sequenceFirst s
        return (first, sequenceRest s)

-- | This is strict. We clearly have to force both parts of the sequence
--   before we can come up with the switched event.
sequenceSwitch
    :: forall f g h t .
       ( Functor f, Functor g )
    => (forall s . f (Event s) -> Event (h s))
    -> (forall s . g (Event s) -> Event (h s))
    -> Sequence f g (Event t)
    -> MomentIO (Event (h t))
sequenceSwitch transF transG sequence = do
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
    e :: f (Event t) <- sequenceFirst sequence
    es :: Event (g (Event t)) <- sequenceRest sequence
    esHasFired :: Behavior Bool <- stepper False (const True <$> es)
    let first :: Event (h t)
        first = transF e
    let rest ::  Event (h t)
        rest = switchE (transG <$> es)
    return $ unionWith const 
                       (filterApply ((const . not) <$> esHasFired) first)
                       (filterApply (const <$> esHasFired) rest)

-- | Like switchE but when the events are in an SEvent.
sSwitchE :: SEvent (Event t) -> MomentIO (Event t)
sSwitchE = (fmap . fmap) runIdentity . sequenceSwitch (\_ -> never) (fmap Identity . runIdentity)

-- | Like sequenceSwitch'' but we hide the MomentIO computation inside the
--   Sequence, and gives Const () (i.e. nothing) for the initial value.
sequenceSwitch'
    :: forall f g h t .
       ( Functor f, Functor g )
    => (forall s . f (Event s) -> Event (h s))
    -> (forall s . g (Event s) -> Event (h s))
    -> Sequence f g (Event t)
    -> Sequence (Const ()) h t
sequenceSwitch' transF transG sequence = Sequence content id
  where
    content :: MomentIO (Const () t, MomentIO (Event (h t)))
    content = return (Const (), sequenceSwitch transF transG sequence)

-- | A highly general variant of @switchE :: Event (Event t) -> Event t@.
--   We recover something like @switchE@ by setting
--
--     @
--       f1 ~ Const ()
--       f2 ~ Const ()
--       f3 ~ Const ()
--       g1 ~ Identity
--       g2 ~ Identity
--       g3 ~ Maybe?
--     @
--
--   to obtain @SEvent (SEvent t) -> SEvent t@
switchSequence
    :: forall f1 f2 f3 g1 g2 g3 t .
       ( Functor f1
       , Functor f2
       , Functor f3
       , Functor g1
       , Functor g2
       , Functor g3
       )
    => (forall t . f1 (MomentIO t) -> MomentIO (f1 t))
    -> (forall t . g1 (MomentIO t) -> MomentIO (g1 t))
    -> (forall t . f1 (f2 t) -> f3 t) -- ^ The immediate part of the output
                                      --   sequence is computed through the
                                      --   immediate part functors of both.
                                      --   If one of f1, f2 is Const (), for
                                      --   instance, then f3 must be Const(),
                                      --   indicating that if either sequence
                                      --   has no immediate part, then the
                                      --   resulting sequence has no immediate
                                      --   part.
    -> (forall t . f1 (Event (g2 t)) -> Event (g3 t))
    -> (forall t . Event (g1 (f2 t)) -> Event (g3 t))
    -> (forall t . g1 (Event (g2 t)) -> Event (g3 t))
    -> (forall t . g3 t -> g3 t -> g3 t)
    -> Sequence f1 g1 (Sequence f2 g2 t)
    -> Sequence f3 g3 t
switchSequence commuteF1 commuteG1 joinF1F2 eventF1 eventG1F2 eventG1 disambiguatorG3 sequence = Sequence content id
  where
    content :: MomentIO (f3 t, MomentIO (Event (g3 t)))
    content = do

        first :: f1 (Sequence f2 g2 t) <- sequenceFirst sequence
        first' :: f1 (f2 t) <- commuteF1 (sequenceFirst <$> first)

        let rest' :: MomentIO (Event (g3 t))
            rest' = do
                rest :: Event (g1 (Sequence f2 g2 t)) <- sequenceRest sequence
                -- The remaining elements of the first sequence.
                firstRest :: f1 (Event (g2 t))
                    <- commuteF1 (sequenceRest <$> first)
                -- The first (immediate) elements of the remaining sequences.
                restFirst :: Event (g1 (f2 t))
                    <- execute ((commuteG1 . fmap sequenceFirst) <$> rest)
                -- The remaining elements of the remaining sequences.
                restRest :: Event (g1 (Event (g2 t)))
                    <- execute ((commuteG1 . fmap sequenceRest) <$> rest)
                let switchedRest :: Event (g3 t)
                    switchedRest = switchE (eventG1 <$> restRest)
                -- We want to use firstRest until restFirst has fired at least
                -- once, at which point we union restFirst with restRest.
                restHasFired :: Behavior Bool <- stepper False (const True <$> restFirst)
                let remaining :: Event (g3 t)
                    remaining = unionWith disambiguatorG3
                                          (eventG1F2 restFirst)
                                          (switchedRest)
                return $ unionWith disambiguatorG3
                                   (filterApply ((const . not) <$> restHasFired) (eventF1 firstRest))
                                   (filterApply (const <$> restHasFired) remaining)

        return (joinF1F2 first', rest')

-- Here it is, but will it work?
sSwitchE' :: SEvent (SEvent t) -> SEvent t
sSwitchE' = switchSequence (\_ -> pure (Const ()))
                           (fmap Identity . runIdentity)
                           (\_ -> Const ())
                           (\_ -> never)
                           (\_ -> never)
                           (runIdentity)
                           (\x y -> const <$> x <*> y)

sequenceReactimate
    :: forall f g .
       ( Functor f, Functor g )
    => (f (IO ()) -> IO ())
    -> (g (IO ()) -> IO ())
    -> Sequence f g (IO ())
    -> MomentIO ()
sequenceReactimate elimF elimG sequence = do
    first :: f (IO ()) <- sequenceFirst sequence
    rest :: Event (g (IO ())) <- sequenceRest sequence
    liftIO (elimF first)
    reactimate (elimG <$> rest)
    return ()

immediatelyAfter :: Event t -> MomentIO (Event t)
immediatelyAfter ev = do
    (ev', fire) <- newEvent
    reactimate (fire <$> ev)
    return ev'
