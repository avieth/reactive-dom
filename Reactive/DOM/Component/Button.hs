{-|
Module      : Reactive.DOM.Button
Description : Definition of a Button component.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Reactive.DOM.Component.Button where

import qualified Data.Map as M
import Data.Functor.Identity
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Component
import Reactive.Banana.Frameworks
import GHCJS.Types (JSString)
import GHCJS.DOM.Element as Element (click)

type Button = Simple (SBehavior JSString) (SEvent ())

button :: Button
button = Simple makeButton
  where
    makeButton :: SBehavior JSString -> MomentIO (SEvent (), VirtualElement Identity)
    makeButton textSequence = do
        velem <- virtualElement (pure "button")
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (pure . pure . text . pure <$> textSequence))
        click <- virtualEvent velem Element.click (\_ _ -> return ())
        return (click, velem)
