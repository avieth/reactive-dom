{-|
Module      : Reactive.DOM.TextInput
Description : Definition of a TextInput component.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module Reactive.DOM.Component.TextInput where

import qualified Data.Map as M
import Data.Functor.Identity
import Data.Semigroup (First(..))
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Component
import Reactive.Banana.Frameworks
import GHCJS.Types (JSString)
import GHCJS.DOM.HTMLInputElement (castToHTMLInputElement, getValue)
import GHCJS.DOM.Element as Element (input, change)

-- Output is the sequence of input, and an event fired when the user submits the
-- text.
data TextInput = TextInput

instance IsComponent TextInput where
    type ComponentInputT TextInput = SBehavior JSString
    type ComponentOutputT TextInput = SBehavior JSString
    makeComponent TextInput textSequence = mdo
        -- TODO should use semigroups to ensure that, even if we allow the
        -- programmer to modulate the attributes of a virtual element, that
        -- it can never change the type from text.
        let attributes :: Attributes
            attributes = M.singleton "type" "text"
        let makeProperties :: JSString -> Properties
            makeProperties val = M.singleton "value" val
        velem <- virtualElement (pure "input")
                                (pure (makeProperties <$> outputBehavior))
                                (pure (always attributes))
                                (pure (always M.empty))
                                (pure (always []))
        inputEvent :: SEvent JSString
            <- virtualEvent velem Element.input (\el _ -> maybe "" id <$> getValue (castToHTMLInputElement el))
        let outputBehavior :: SBehavior JSString
            outputBehavior = getFirst <$> ((First <$> textSequence) <||> (First <$> inputEvent))
        pure (outputBehavior, velem)
