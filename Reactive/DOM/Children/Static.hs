{-|
Module      : Reactive.DOM.Children.Static
Description : Definition of the Static higher-order ChildrenContainer
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Reactive.DOM.Children.Static where

import Reactive.DOM.Internal.ChildrenContainer

-- | Derive a ChildrenContainer with no changes.
newtype Static (g :: (* -> *) -> *) (f :: * -> *) = Static {
      runStatic :: g f
    }

data VoidF (f :: * -> *) = VoidF (VoidF f)

absurdF :: VoidF f -> t
absurdF (VoidF voidf) = absurdF voidf

instance FunctorTransformer g => FunctorTransformer (Static g) where
    functorTrans trans (Static x) = Static (functorTrans trans x)
    functorCommute (Static x) = Static <$> functorCommute x

instance FunctorTransformer VoidF where
    functorTrans _ = absurdF
    functorCommute = absurdF

instance ChildrenContainer g => ChildrenContainer (Static g) where
    type Change (Static g) = VoidF
    getChange _ = absurdF
    childrenContainerList get (Static g) = childrenContainerList get g
