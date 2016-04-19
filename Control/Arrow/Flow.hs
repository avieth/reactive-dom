{-|
Module      : Control.Arrow.Flow
Description : Definition of the Flow parametric formal arrow.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}

module Control.Arrow.Flow (

      Flow(..)
    , FlowContinuation(..)
    , flowContinuationSingle
    , flowContinuationTrans
    , flowContinuationTrans'
    , runFlowContinuation
    , FlowTrans
    , flowTrans
    , FlowKleisli
    , flowKleisli
    , continueFlow
    , runFlow

    ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Data.Profunctor

-- | A formal ArrowApply/ArrowChoice with parameterized primitive terms.
data Flow (m :: * -> * -> *) (s :: *) (t :: *) where
    FlowM :: m s t -> Flow m s t
    FlowArr :: (s -> t) -> Flow m s t
    FlowCompose :: Flow m u t -> Flow m s u -> Flow m s t
    FlowFirst :: Flow m s t -> Flow m (s, c) (t, c)
    FlowLeft :: Flow m s t -> Flow m (Either s c) (Either t c)
    FlowApp :: Flow m (Flow m s t, s) t

instance Profunctor (Flow m) where
    dimap l r x = arr l >>> x >>> arr r

instance Category (Flow m) where
    id = arr id
    (.) = FlowCompose

instance Arrow (Flow m) where
    arr = FlowArr
    first = FlowFirst

instance ArrowChoice (Flow m) where
    left = FlowLeft

instance ArrowApply (Flow m) where
    app = FlowApp

type FlowTrans m n = forall s t . m s t -> n s t

-- | Change the primitive terms of a Flow.
flowTrans
    :: forall m n s t .
       ( )
    => FlowTrans m n
    -> Flow m s t
    -> Flow n s t
flowTrans f flow = case flow of
    FlowM m -> FlowM (f m)
    FlowArr g -> FlowArr g
    FlowCompose left right -> FlowCompose (flowTrans f left) (flowTrans f right)
    FlowFirst subFlow -> FlowFirst (flowTrans f subFlow)
    FlowLeft subFlow -> FlowLeft (flowTrans f subFlow)
    FlowApp -> proc (subFlow, s) -> do
        FlowApp -< (flowTrans f subFlow, s)

-- | The free monad over n.
data FlowContinuation (n :: * -> *) (t :: *) where
    FlowDone :: t -> FlowContinuation n t
    FlowNext :: n (FlowContinuation n t) -> FlowContinuation n t

instance Functor n => Functor (FlowContinuation n) where
    fmap f term = case term of
        FlowDone t -> FlowDone (f t)
        FlowNext next -> FlowNext ((fmap . fmap) f next)

instance Functor n => Applicative (FlowContinuation n) where
    pure = FlowDone
    mf <*> mx = do
        f <- mf
        x <- mx
        pure (f x)

instance Functor n => Monad (FlowContinuation n) where
    return = pure
    (mx :: FlowContinuation n s) >>= (k :: s -> FlowContinuation n t) = case mx of
        FlowDone x -> k x
        FlowNext (nx :: n (FlowContinuation n s)) ->
            let k' :: FlowContinuation n s -> FlowContinuation n t
                k' = flip (>>=) k
            in  FlowNext (k' <$> nx)

flowContinuationSingle :: Functor n => n t -> FlowContinuation n t
flowContinuationSingle n = FlowNext (fmap FlowDone n)

-- | Change the functor part of a FlowContinuation.
flowContinuationTrans
    :: forall n m t .
       ( Functor m )
    => (forall t . n t -> m t)
    -> FlowContinuation n t
    -> FlowContinuation m t
flowContinuationTrans f term = case term of
    FlowDone t -> FlowDone t
    FlowNext n -> FlowNext (fmap (flowContinuationTrans f) (f n))

flowContinuationTrans'
    :: forall n m t .
       ( Functor m )
    => (forall t p . n (FlowContinuation p t) -> m (FlowContinuation p t))
    -> FlowContinuation n t
    -> FlowContinuation m t
flowContinuationTrans' trans term = case term of
    FlowDone t -> FlowDone t
    FlowNext n -> FlowNext (fmap (flowContinuationTrans' trans) (trans n))

-- | A formal Flow over an ArrowApply with ArrowChoice can be moved into that
--   Arrow. This one is in a continuation-passing form. See also runFlow.
continueFlow
    :: forall n s t u .
       ( ArrowApply n, ArrowChoice n )
    => Flow n s u
    -> n u t
    -> n s t
continueFlow flow k = case flow of
    FlowM m -> k . m
    FlowArr f -> k . arr f
    FlowCompose left right -> continueFlow right (continueFlow left k)
    FlowFirst subFlow -> proc (s, c) -> do
        t <- continueFlow subFlow id -< s
        k -< (t, c)
    FlowLeft subFlow -> proc choice -> do
        t <- continueFlow subFlow id +++ id -< choice
        k -< t
    FlowApp -> proc (subFlow, s) -> do
        t <- app -< (continueFlow subFlow id, s)
        k -< t

-- | Interpret a Flow into some arrow n.
runFlow
    :: forall m n s t .
       ( ArrowApply n, ArrowChoice n )
    => Flow n s t
    -> n s t
runFlow = flip continueFlow id

type FlowKleisli n = Kleisli (FlowContinuation n)

flowKleisli
    :: ( Functor n )
    => (forall s t . m s t -> s -> n t)
    -> FlowTrans m (FlowKleisli n)
flowKleisli f = \m -> Kleisli $ \s -> FlowNext (fmap FlowDone (f m s))

runFlowContinuation :: Monad n => FlowContinuation n t -> n t
runFlowContinuation (FlowDone t) = pure t
runFlowContinuation (FlowNext next) = next >>= runFlowContinuation
