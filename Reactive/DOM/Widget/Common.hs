{-|
Module      : Reactive.DOM.Widget.Common
Description : Various useful widgets
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Reactive.DOM.Widget.Common where

import qualified Data.Text as T
import Reactive.DOM.Node
import Reactive.DOM.Children.Static
import Reactive.DOM.Children.NodeList
import Reactive.DOM.Children.Single
import Reactive.Banana.Combinators
import Reactive.Sequence
import Data.Profunctor

varyingText :: OpenWidget (T.Text, Event T.Text) ()
varyingText = widget $ \(~((txt, evTxt), _)) ->
    pure ((), children (Single (textChild txt)) (pure . Single . textChild <$> evTxt))

constantText :: OpenWidget T.Text ()
constantText = lmap (\t -> (t, never)) varyingText

div :: OpenWidget s t -> Widget "div" s t
div = id

p :: OpenWidget s t -> Widget "p" s t
p = id

span :: OpenWidget s t -> Widget "span" s t
span = id

button :: OpenWidget s t -> Widget "button" s t
button = id

input :: OpenWidget s t -> Widget "input" s t
input = id

form :: OpenWidget s t -> Widget "form" s t
form = id

hr :: Widget "hr" () ()
hr = trivialWidget

-- | A button which shows text and gives a click event.
simpleButton :: Widget "button" T.Text (Event ())
simpleButton = button constantText `modify` modifier (const (event Click))

-- | The text displayed here is not determined by children, but by the value
--   property, so it can be played with through the Modification interface.
textInput :: Widget "input" () (Event T.Text)
textInput = input trivialWidget `modify` modifier (const (event Input))

-- | Same as textInput but we set the type attribute to password for you.
passwordInput :: Widget "input" () (Event T.Text)
passwordInput = textInput `modify` modifier setPasswordType
  where
    setPasswordType event = attributes (always (Set attrs)) >> pure event
    attrs = makeAttributes [("type", "password")]
