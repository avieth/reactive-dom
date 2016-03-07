{-|
Module      : Reactive.DOM.Children.MonotoneList
Description : Definition of the MonotoneList children container.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Reactive.DOM.Children.MonotoneList where

import Data.Semigroup
import Data.Functor.Compose
import Reactive.DOM.Internal.ChildrenContainer
import Reactive.DOM.Internal.Mutation

newtype MonotoneList t inp out f = MonotoneList {
      runMonotoneList :: [f t]
    }

deriving instance Semigroup (MonotoneList t inp out f)
deriving instance Monoid (MonotoneList t inp out f)

instance FunctorTransformer (MonotoneList inp out t) where
    functorTrans trans (MonotoneList fts) = MonotoneList (trans <$> fts)
    functorCommute (MonotoneList fts) = MonotoneList <$> sequenceA (getCompose <$> fts)

instance ChildrenContainer (MonotoneList inp out t) where
    type Change (MonotoneList t inp out) = MonotoneList t inp out
    getChange get (MonotoneList news) (MonotoneList olds) =
        let nextList = MonotoneList (olds <> news)
            mutations = AppendChild . get <$> news
        in  (nextList, mutations)
    childrenContainerList get (MonotoneList ts) = get <$> ts
