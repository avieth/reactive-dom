{-|
Module      : Reactive.DOM.Widget.Primitive
Description : Various useful primitive widgets
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE OverloadedStrings #-}

module Reactive.DOM.Widget.Primitive where

import qualified Data.Text as T
import Reactive.DOM.Node
import Reactive.DOM.Children.Static
import Reactive.DOM.Children.NodeList
import Reactive.Banana.Combinators
import Reactive.Sequence

emptyWidget :: Widget () ()
emptyWidget = widget $ \_ -> pure ((), constantChildren (Static (nodeList [])))

constantText :: T.Text -> Widget () ()
constantText txt = widget $ \_ ->
    pure ((), constantChildren (Static (nodeList [textChild txt])))

varyingText :: (T.Text, Event T.Text) -> Widget () ()
varyingText (txt, evTxt) = widget $ \_ ->
    pure ((), children (nodeList [textChild txt]) (pure . nodeList . pure . textChild <$> evTxt))

div :: Widget () t -> UI t
div = ui "div"

p :: Widget () t -> UI t
p = ui "p"

span :: Widget () t -> UI t
span = ui "span"

button :: Widget () t -> UI t
button = ui "button"

-- | A button which shows text and gives a click event.
simpleButton :: T.Text -> UI (Event ())
simpleButton txt = button (constantText txt) `modify` modifier (const (event Click))

-- | The text displayed here is not determined by children, but by the value
--   property, so it can be played with through the Modification interface.
textInput :: UI (Event T.Text)
textInput = ui "input" emptyWidget `modify` modifier (const (event Input))

-- | Same as textInput but we set the type attribute to password for you.
passwordInput :: UI (Event T.Text)
passwordInput = textInput `modify` modifier setPasswordType
  where
    setPasswordType event = attributes (always (Set attrs)) >> pure event
    attrs = makeAttributes [("type", "password")]
