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
{-# LANGUAGE TypeFamilies #-}
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

data Button = Button

instance IsComponent Button where
    type ComponentInputT Button = JSString
    type ComponentOutputT Button = ()
    makeComponent Button labelText = do
        velem <- virtualElement (pure "button")
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (pure . text . pure <$> (always labelText)))
        return ((), velem)
