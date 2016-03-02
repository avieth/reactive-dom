{-|
Module      : Reactive.DOM.Children.Cardinality
Description : Children sets of known cardinality.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Reactive.DOM.Children.Cardinality (

      Cardinality(..)
    , cardinality
    , setChildAtIndex
    , module Data.TypeNat.Nat
    , module Data.TypeNat.Fin
    , module Data.TypeNat.Vect

    ) where

import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.TypeNat.Nat
import Data.TypeNat.Vect
import Data.TypeNat.Fin
import Data.List.NonEmpty
import Reactive.DOM.Internal.Mutation
import Reactive.DOM.Internal.Children

newtype Cardinality (n :: Nat) t = Cardinality {
      runCardinality :: Vect n t
    }

deriving instance Functor (Cardinality n)
deriving instance Foldable (Cardinality n)
deriving instance Traversable (Cardinality n)

type NoChildren = Cardinality Zero

cardinality :: Vect n t -> Cardinality n t
cardinality = Cardinality

-- Changes are 1 or more indexed replacements.
-- Notice that Change (Cardinality Zero) = NonEmpty (Fin Zero, t) = Void
-- because there is no Fin Zero.
newtype CardinalityChange (n :: Nat) t = CardinalityChange {
      runCardinalityChange :: NonEmpty (Fin n, t)
    }

deriving instance Functor (CardinalityChange n)
deriving instance Foldable (CardinalityChange n)
deriving instance Traversable (CardinalityChange n)
deriving instance Semigroup (CardinalityChange n t)

setChildAtIndex :: Fin n -> t -> CardinalityChange n t
setChildAtIndex idx t = CardinalityChange ((idx, t) :| [])

instance ChildrenContainer (Cardinality n) where
    type Change (Cardinality n) = CardinalityChange n
    runChange change x = foldl changeOne (x, []) (runCardinalityChange change)
      where
        changeOne (Cardinality vect, mutations) (idx, new) =
            let old = safeIndex idx vect
                nextVect = safeUpdate idx (const new) vect
            in  (Cardinality nextVect, ReplaceChild new old : mutations)
