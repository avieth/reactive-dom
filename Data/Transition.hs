{-|
Module      : Data.Transition
Description : Transition datatype; a special kind of functor.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Transition (

      Transition(..)
    , transTransition
    , splitTransition
    , runTransition
    , takeTransition

    , SimpleTransition
    , simpleTransition

    ) where

import Prelude hiding ((.), id)
import Control.Category
import Data.Functor.Identity
import Data.Bifunctor (bimap)

newtype Transition d t = Transition {
      getTransition :: (d, t)
    }

instance Functor (Transition d) where
    fmap f = Transition . fmap f . getTransition

instance Monoid d => Applicative (Transition d) where
    pure t = Transition (mempty, t)
    Transition (d1, f) <*> Transition (d2, x) = Transition (d1 `mappend` d2, f x)

instance Monoid d => Monad (Transition d) where
    return = pure
    Transition (d1, x) >>= k =
        let Transition (d2, y) = k x
        in  Transition (d1 `mappend` d2, y)

transTransition :: (d1 -> d2) -> Transition d1 t -> Transition d2 t
transTransition trans (Transition (d1, t)) = Transition (trans d1, t)

splitTransition
    :: forall d s t .
       Transition d (Either s t)
    -> Either (Transition d s) (Transition d t)
splitTransition (Transition (d, choice)) = bimap mk mk choice
  where
    mk :: forall s . s -> Transition d s
    mk = Transition . (,) d

runTransition :: Transition d t -> t
runTransition = snd . getTransition

takeTransition :: Transition d t -> d
takeTransition = fst . getTransition

type SimpleTransition = Transition ()

simpleTransition :: t -> SimpleTransition t
simpleTransition t = Transition ((), t)
