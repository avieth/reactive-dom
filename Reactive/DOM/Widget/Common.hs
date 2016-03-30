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
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Data.Profunctor

varyingText :: OpenWidget (Sequence T.Text) ()
varyingText = widget $ \(~(seqnc, _)) -> do
    let ~(initial, rest) = runSequence seqnc
    pure ((), children (Single (textChild initial)) (pure . Single . textChild <$> rest))

constantText :: OpenWidget T.Text ()
constantText = lmap always varyingText

div :: OpenWidget s t -> Widget "div" s t
div = id

a :: OpenWidget s t -> Widget "a" s t
a = id

link :: OpenWidget s t -> Widget "a" (s, T.Text) t
link w = pullthrough (a (lmap fst w)) `modifyr` setHref
  where
    setHref = modifier $ \((_, href), t) -> do
        attributes' (always [Set (makeAttributes [("href", href)])])
        pure t

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

br :: Widget "br" () ()
br = trivialWidget

-- | A button which shows text and gives a click event.
simpleButton :: Widget "button" T.Text (Event ())
simpleButton = button constantText `modifyr` modifier (const (event Click))

-- | The text displayed here is not determined by children, but by the value
--   property, so it can be played with through the Modification interface.
textInput :: Widget "input" () (Event T.Text)
textInput = input trivialWidget `modifyr` modifier (const (event Input))

textInputVarying :: Widget "input" (Sequence T.Text) (Event T.Text)
textInputVarying = pullthrough (input (lmap (const ()) trivialWidget)) `modifyr` setup
  where
    setup = modifier $ \(s, t) -> setInputValue s t >> event Input

setInputValue :: Sequence T.Text -> () -> ElementBuilder "input" ()
setInputValue seqnc _ = do
    -- Each text gets its own Properties, and we're careful to unset the
    -- previous one.
    let uniqueProperties = setValue <$> seqnc
    let ~(initial, _) = runSequence uniqueProperties
    changes <- sequenceChanges uniqueProperties
    properties' ([Set initial] |> (unsetSet <$> changes))
    pure ()
  where
    setValue :: T.Text -> Properties
    setValue txt = makeProperties [("value", txt)]
    unsetSet :: (Properties, Properties) -> [Action Properties]
    unsetSet (old, new) = [Unset old, Set new]

-- | Same as textInput but we set the type attribute to password for you.
passwordInput :: Widget "input" () (Event T.Text)
passwordInput = textInput `modifyr` modifier setPasswordType
  where
    setPasswordType event = attributes (always (Set attrs)) >> pure event
    attrs = makeAttributes [("type", "password")]

passwordInputVarying :: Widget "input" (Sequence T.Text) (Event T.Text)
passwordInputVarying = pullthrough (input (lmap (const ()) trivialWidget)) `modifyr` setup
  where
    attrs = makeAttributes [("type", "password")]
    setup = modifier $ \(s, t) -> do
        attributes (always (Set attrs))
        setInputValue s t 
        event Input
