{-|
Module      : Data.Union
Description : Binary union datatype: left, right, or both.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Data.Union (

      Union(..)
    , elimUnion
    , eleft
    , eright
    , esplit 
    , eunion
    , uswap
    , pickULeft
    , pickURight
    , uGrowLeft
    , uGrowRight

    ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow.Flow
import Data.Profunctor
import Data.Bifunctor
import Data.Semigroup (Semigroup, Endo(..), (<>))
import Data.Monoid hiding ((<>))
import Reactive.Banana.Combinators

-- | Ternary datatype: one, the other, or both.
data Union s t = ULeft s | URight t | UBoth s t

instance Bifunctor Union where
    bimap f g term = case term of
        ULeft s -> ULeft (f s)
        URight t -> URight (g t)
        UBoth s t -> UBoth (f s) (g t)

instance Functor (Union s) where
    fmap = second

instance Semigroup s => Applicative (Union s) where
    pure = URight
    mf <*> mx = do
        f <- mf
        x <- mx
        pure (f x)

instance Semigroup s => Monad (Union s) where
    return = pure
    mx >>= k = joinUnion (fmap k mx)

uswap :: Union s t -> Union t s
uswap term = case term of
    ULeft s -> URight s
    URight t -> ULeft t
    UBoth s t -> UBoth t s

elimUnion :: (s -> s -> s) -> Union s s -> s
elimUnion _ (ULeft s) = s
elimUnion _ (URight s) = s
elimUnion f (UBoth s1 s2) = f s1 s2

pickULeft :: Union s t -> Maybe s
pickULeft (ULeft s) = Just s
pickULeft (UBoth s _) = Just s
pickULeft (URight _) = Nothing

pickURight :: Union s t -> Maybe t
pickURight (URight t) = Just t
pickURight (UBoth _ t) = Just t
pickuRight (ULeft _) = Nothing

joinUnion :: Semigroup s => Union s (Union s t) -> Union s t
joinUnion (ULeft s) = ULeft s
joinUnion (URight (URight t)) = URight t
joinUnion (URight (ULeft s)) = ULeft s
joinUnion (URight (UBoth s t)) = UBoth s t
joinUnion (UBoth s (URight t)) = UBoth s t
joinUnion (UBoth s (ULeft s')) = ULeft (s <> s')
joinUnion (UBoth s (UBoth s' t)) = UBoth (s <> s') t

eunion :: forall s t . Event s -> Event t -> Event (Union s t)
eunion ss ts =
    let left :: Event (Union s t)
        left = ULeft <$> ss
        right :: Event (Union s t)
        right = URight <$> ts
        -- Partial match is safe (if it's not, reactive-banana is at fault).
        combine (ULeft s) (URight t) = UBoth s t
    in  unionWith combine left right

esplit :: Event (Union s t) -> (Event s, Event t)
esplit ev =
    let lefts = filterJust (pickULeft <$> ev)
        rights = filterJust (pickURight <$> ev)
    in  (lefts, rights)

eleft :: Event (Union s t) -> Event s
eleft = fst . esplit

eright :: Event (Union s t) -> Event t
eright = snd . esplit

uGrowLeft :: (t -> Maybe u) -> Union s t -> Union (Union s u) t
uGrowLeft f term = case term of
    ULeft s -> ULeft (ULeft s)
    URight t -> case f t of
        Nothing -> URight t
        Just u -> UBoth (URight u) t
    UBoth s t -> case f t of
        Nothing -> UBoth (ULeft s) t
        Just u -> UBoth (UBoth s u) t

uGrowRight :: (s -> Maybe u) -> Union s t -> Union s (Union t u)
uGrowRight f term = uswap (uGrowLeft f (uswap term))
