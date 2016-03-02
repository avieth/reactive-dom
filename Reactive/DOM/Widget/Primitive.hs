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
