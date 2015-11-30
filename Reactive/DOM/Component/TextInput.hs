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
import qualified Data.Text as T
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Component
import Reactive.Banana.Frameworks
import GHCJS.DOM.HTMLInputElement (castToHTMLInputElement, getValue)
import GHCJS.DOM.Element as Element (input, change)

-- Output is the sequence of input, and an event fired when the user submits the
-- text.
data TextInput = TextInput

instance IsComponent TextInput where
    type ComponentInputT TextInput = SBehavior T.Text
    type ComponentOutputT TextInput = SBehavior T.Text
    makeComponent TextInput textSequence = mdo
        -- TODO should use semigroups to ensure that, even if we allow the
        -- programmer to modulate the attributes of a virtual element, that
        -- it can never change the type from text.
        let attributes :: Attributes
            attributes = MapWithPrecedence $ M.singleton "type" (Immutable "text")
        let makeProperties :: T.Text -> Properties
            -- NB the Immutable here just means that if you try to merge
            -- other properties into this, you can't alter the value.
            -- It can still change, though, whenever the outputBehavior fires.
            makeProperties val = MapWithPrecedence $ M.singleton "value" (Immutable val)
        velem <- virtualElement (pure "input")
                                (pure (makeProperties <$> outputBehavior))
                                (pure (always attributes))
                                (pure (always mempty))
                                (pure (always []))
        inputEvent :: SEvent T.Text
            <- virtualEvent velem Element.input (\el _ -> maybe "" id <$> getValue (castToHTMLInputElement el))
        let outputBehavior :: SBehavior T.Text
            outputBehavior = getFirst <$> ((First <$> textSequence) <||> (First <$> inputEvent))
        pure (outputBehavior, velem)
