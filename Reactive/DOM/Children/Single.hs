{-|
Module      : Reactive.DOM.Children.Single
Description : Definition of the Single children container.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE TypeFamilies #-}

module Reactive.DOM.Children.Single where

import Data.Functor.Compose
import Reactive.DOM.Internal.ChildrenContainer
import Reactive.DOM.Internal.Mutation

newtype Single t inp out f = Single {
      runSingle :: f t
    }

instance FunctorTransformer (Single t inp out) where
    functorTrans trans (Single x) = Single (trans x)
    functorCommute (Single compose) = Single <$> getCompose compose

instance ChildrenContainer (Single t inp out) where
    type Change (Single t inp out) = Single t inp out
    getChange get (Single new) (Single old) = (Single new, [ReplaceChild (get new) (get old)])
    childrenContainerList get (Single x) = [get x]
