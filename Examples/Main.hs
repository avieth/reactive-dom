{-|
Module      : Example.Main
Description : Show various examples.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

import Prelude hiding ((.), id, div)
import Control.Category
import Control.Arrow
import Data.Void
import qualified Data.Text as T
import GHCJS.DOM
import GHCJS.DOM.Window
import GHCJS.DOM.Document
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Widget.Primitive
import Examples.Calculator

main :: IO ()
main = runWebGUI $ \webView -> do

    Just document <- webViewGetDomDocument webView
    Just body <- getBody document

    let networkDescription :: MomentIO ()
        networkDescription = do

            (_, calculatorEvent) <- render document body (ui (div calculator))
            reactimate (Prelude.print <$> calculatorEvent)

            pure ()

    network <- compile networkDescription
    actuate network

    pure ()
