{-|
Module      : Reactive.Eventful
Description : Definition of the Eventful monad.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

DEPRECATED
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Reactive.Eventful (

      Eventful
    , eventful
    , runEventful
    , initial
    , andThen
    , (||>)

    , ComposableEvent
    , composableEvent
    , runComposableEvent

    , ArrowEvent
    , runArrowEvent

    ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.EitherBoth
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

-- | @MomentIO@ computation with an assumed *initial event*, which fires at the
--   beginning of time. Of course, you are required to provide this initial
--   event when you @runEventful@, so you get to decide precisely when--and
--   how often--the beginning of time happens.
newtype Eventful t = Eventful {
      outEventful :: ReaderT (Event ()) MomentIO t
    }

deriving instance Functor Eventful
deriving instance Applicative Eventful
deriving instance Monad Eventful
deriving instance MonadFix Eventful
deriving instance MonadIO Eventful

eventful :: MomentIO t -> Eventful t
eventful = Eventful . lift

runEventful :: Eventful t -> Event () -> MomentIO t
runEventful = runReaderT . outEventful

initial :: t -> Eventful (Event t)
initial t = Eventful $ do
    i <- ask
    return (const t <$> i)

andThen :: t -> Event t -> Eventful (Event t)
andThen first rest = do
    evFirst <- initial first
    return (unionWith const rest evFirst)

(||>) = andThen

-- | Like an Event, but with an Applicative instance!
newtype ComposableEvent t = ComposableEvent {
      outComposableEvent :: Eventful (Event t)
    }

deriving instance Functor ComposableEvent

instance Applicative ComposableEvent where
    pure = ComposableEvent . initial
    mf <*> mx = uncurry ($) <$> bundle mf mx

composableEvent :: Eventful (Event t) -> ComposableEvent t
composableEvent = ComposableEvent

runComposableEvent :: ComposableEvent t -> Eventful (Event t)
runComposableEvent = outComposableEvent

-- | Like an Event, but with an Arrow instance whenever @a@ is an Arrow!
newtype ArrowEvent a s t = ArrowEvent {
      outArrowEvent :: Eventful (Event (a s t))
    }

instance Category a => Category (ArrowEvent a) where
    id = ArrowEvent (initial id)
    left . right = ArrowEvent $ do let left' = composableEvent (outArrowEvent left)
                                   let right' = composableEvent (outArrowEvent right)
                                   let composed = (.) <$> left' <*> right'
                                   runComposableEvent composed

instance Arrow a => Arrow (ArrowEvent a) where
    arr = ArrowEvent . initial . arr
    first ar = ArrowEvent $ do let ar' = composableEvent (outArrowEvent ar)
                               let composed = first <$> ar'
                               runComposableEvent composed

instance ArrowChoice a => ArrowChoice (ArrowEvent a) where
    left ar = ArrowEvent $ do let ar' = composableEvent (outArrowEvent ar)
                              let composed = left <$> ar'
                              runComposableEvent composed

runArrowEvent :: ArrowEvent a s t -> Eventful (Event (a s t))
runArrowEvent = outArrowEvent

-- | This is an equivalent formulation of @<*>@ for @ComposableEvent@:
--
--     @mf <*> mx = uncurry ($) <$> bundle mf mx@
bundle
    :: forall s t .
       ComposableEvent s
    -> ComposableEvent t
    -> ComposableEvent (s, t)
bundle left right = ComposableEvent $ mdo
    evS :: Event s <- outComposableEvent left
    evT :: Event t <- outComposableEvent right
    bs :: Behavior (Maybe s) <- Eventful . lift $ stepper Nothing (Just <$> evS)
    bt :: Behavior (Maybe t) <- Eventful . lift $ stepper Nothing (Just <$> evT)
    -- Now we create an event which merges s and t, handling the case in which
    -- they fire simultaneously by using the EitherBoth type.
    let evSDiscriminated :: Event (EitherBoth s t)
        evSDiscriminated = OneLeft <$> evS
    let evTDiscriminated :: Event (EitherBoth s t)
        evTDiscriminated = OneRight <$> evT
    let evST :: Event (EitherBoth s t)
        evST = unionWith (\(OneLeft s) (OneRight t) -> Both s t)
                         (evSDiscriminated)
                         (evTDiscriminated)
    -- We throw in the behaviors for s and t, yielding an event which contains
    -- all the information we need in order to decide when to fire.
    let evST' :: Event ((Maybe s, Maybe t), EitherBoth s t)
        evST' = (,) <$> ((,) <$> bs <*> bt) <@> evST
    let pick :: ((Maybe s, Maybe t), EitherBoth s t) -> Maybe (s, t)
        pick x = case x of
            -- If both fire, just give them.
            (_, Both s t) -> Just (s, t)
            -- If only one fires and the other has already fired, give the
            -- latest tuple.
            ((_, Just t), OneLeft s) -> Just (s, t)
            ((Just s, _), OneRight t) -> Just (s, t)
            -- If only one fires and the other has not previously fired, we just
            -- don't have enough information!
            _ -> Nothing
    return (filterJust (pick <$> evST'))
