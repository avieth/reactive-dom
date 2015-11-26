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

module Reactive.DOM.Component.Label where

import qualified Data.Map as M
import Data.Functor.Identity
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Component
import Reactive.Banana.Frameworks
import GHCJS.Types (JSString)

type Label = Simple (SBehavior JSString) (SBehavior JSString)

label :: Label
label = Simple makeLabel
  where
    makeLabel :: SBehavior JSString -> MomentIO (SBehavior JSString, VirtualElement Identity)
    makeLabel textSequence = do
        velem <- virtualElement (pure "span")
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (pure . pure . text . pure <$> textSequence))
        return (textSequence, velem)
