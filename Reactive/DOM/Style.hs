{-|
Module      : Reactive.DOM.Style
Description : Definition of some common styles.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE OverloadedStrings #-}

module Reactive.DOM.Style where

import Reactive.DOM.Node
import Data.Semigroup

fullWidth :: Style
fullWidth = makeStyle [("width", "100%")]

fullHeight :: Style
fullHeight = makeStyle [("height", "100%")]

fullWidthHeight :: Style
fullWidthHeight = fullWidth <> fullHeight
