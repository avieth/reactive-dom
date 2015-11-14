{-|
Module      : Data.EitherBoth
Description : Definition of the EitherBoth bifunctor
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Data.EitherBoth (

      EitherBoth(..)
    , eitherBoth

    ) where

import Data.Monoid
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

data EitherBoth a b = OneLeft a | OneRight b | Both a b

eitherBoth :: (a -> c) -> (b -> c) -> (a -> b -> c) -> EitherBoth a b -> c
eitherBoth l r b e = case e of
    OneLeft x -> l x
    OneRight x -> r x
    Both x y -> b x y

instance Bifunctor EitherBoth where
    bimap l r e = case e of
        OneLeft a -> OneLeft (l a)
        OneRight b -> OneRight (r b)
        Both a b -> Both (l a) (r b)

instance Bifoldable EitherBoth where
    bifoldMap l r e = case e of
        OneLeft a -> l a
        OneRight b  -> r b
        Both a b -> (l a) <> (r b)

instance Bitraversable EitherBoth where
    bitraverse l r e = case e of
        OneLeft a -> OneLeft <$> l a
        OneRight b -> OneRight <$> r b
        Both a b -> Both <$> l a <*> r b


