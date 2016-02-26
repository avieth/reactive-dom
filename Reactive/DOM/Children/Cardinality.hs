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

module Reactive.DOM.Children.Cardinality (

      Cardinality
    , NoChildren
    , noChildren
    , cardinality
    , cardinalitySet
    , module Data.TypeNat.Nat
    , module Data.TypeNat.Fin
    , module Data.TypeNat.Vect

    ) where

import Data.TypeNat.Nat
import Data.TypeNat.Vect
import Data.TypeNat.Fin
import Reactive.DOM.Internal.Mutation

newtype Cardinality (n :: Nat) t = Cardinality {
      runCardinality :: Vect n t
    }

type NoChildren = Cardinality Zero

noChildren :: Cardinality Zero t
noChildren = Cardinality VNil

cardinality :: Vect n t -> Mutation t (Cardinality Zero) (Cardinality n)
cardinality vect = Mutation $ \_ -> (Cardinality vect, AppendChild <$> vectToList vect)

cardinalitySet :: Fin n -> (t -> t) -> Automutation t (Cardinality n)
cardinalitySet idx f = Mutation $ \(Cardinality vect) -> 
    let old = safeIndex idx vect
        new = f old
    in  (Cardinality (safeUpdate idx (const new) vect), [ReplaceChild new old])
