{-|
Module      : Example.Main
Description : Show various examples.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

import GHCJS.DOM
import GHCJS.DOM.Window
import GHCJS.DOM.Document
import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.DOM.Node
import Reactive.DOM.Widget
import Examples.Counter
import Examples.Simple

main :: IO ()
main = runWebGUI $ \webView -> do

    Just document <- webViewGetDomDocument webView
    Just body <- getBody document

    let networkDescription :: MomentIO ()
        networkDescription = do

            (ev, velem) <- runWidget <$> counter 0
            _ <- render document body velem
            reactimate (Prelude.print <$> ev)

            (ev', velem') <- runWidget <$> ourInputBox
            _ <- render document body velem'
            reactimate (Prelude.print <$> ev')

            pure ()

    network <- compile networkDescription
    actuate network

    pure ()
