{-|
Module      : Reactive.DOM.Internal.ChildrenContainer
Description : Definition of the ChildrenContainer class.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Reactive.DOM.Internal.ChildrenContainer where

import Data.Functor.Compose
import Reactive.DOM.Internal.Mutation

-- TODO reconsider the name...
-- HigherOrderFunctor?
class FunctorTransformer (f :: (* -> *) -> *) where
    functorTrans :: (forall t . g t -> h t) -> f g -> f h
    functorCommute :: Applicative m => f (Compose m g) -> m (f g)

class
    ( FunctorTransformer f
    , FunctorTransformer (Change f)
    ) => ChildrenContainer (f :: (* -> *) -> *)
  where
    type Change f :: (* -> *) -> *
    getChange
        :: ( Ord t )
        => (forall s . g s -> t)
        -> Change f g
        -> f g
        -> (f g, [ChildrenMutation t t])
    childrenContainerList :: (forall s . g s -> t) -> f g -> [t]

-- Run a list of changes, applying the leftmost (head of list) first.
runChanges
    :: (Change f g -> f g -> (f g, [ChildrenMutation t t]))
    -> f g
    -> [Change f g]
    -> (f g, [ChildrenMutation t t])
runChanges f current cs = case cs of
    [] -> (current, [])
    (c : rest) ->
        let (next, mutations) = f c current
            (final, mutations') = runChanges f next rest
        in  (final, mutations ++ mutations')
