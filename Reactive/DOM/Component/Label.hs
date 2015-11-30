{-|
Module      : Reactive.DOM.Label
Description : Definition of a Label component.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Reactive.DOM.Component.Label where

import qualified Data.Map as M
import Data.Functor.Identity
import Data.Semigroup
import qualified Data.Text as T
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Component
import Reactive.Banana.Frameworks
import GHCJS.Types (JSString)

data Label = Label

instance IsComponent Label where
    type ComponentInputT Label = T.Text
    type ComponentOutputT Label = ()
    makeComponent Label labelText = do
        velem <- virtualElement (pure "span")
                                (pure (always mempty))
                                (pure (always mempty))
                                (pure (always mempty))
                                (pure (pure . text . pure <$> always labelText))
        return ((), velem)
