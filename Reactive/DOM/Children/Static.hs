{-|
Module      : Reactive.DOM.Children.Static
Description : Definition of the Static higher-order ChildrenContainer
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Reactive.DOM.Children.Static where

import Reactive.DOM.Internal.ChildrenContainer

-- | Derive a ChildrenContainer with no changes.
newtype Static (g :: l -> k -> (* -> *) -> *) (inp :: l) (out :: k) (f :: * -> *) = Static {
      runStatic :: g inp out f
    }

data VoidF (f :: * -> *) = VoidF (VoidF f)

absurdF :: VoidF f -> t
absurdF (VoidF voidf) = absurdF voidf

instance FunctorTransformer (g inp out) => FunctorTransformer (Static g inp out) where
    functorTrans trans (Static x) = Static (functorTrans trans x)
    functorCommute (Static x) = Static <$> functorCommute x

instance FunctorTransformer VoidF where
    functorTrans _ = absurdF
    functorCommute = absurdF

instance ChildrenContainer (g inp out) => ChildrenContainer (Static g inp out) where
    type Change (Static g inp out) = VoidF
    getChange _ = absurdF
    childrenContainerList get (Static g) = childrenContainerList get g
