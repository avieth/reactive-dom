{-|
Module      : Reactive.DOM.Window
Description : Definition of Window-related reactive DOM things.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Reactive.DOM.Window where

import Control.Monad.IO.Class
import GHCJS.DOM.Window as Window
import GHCJS.DOM.EventM hiding (Event)
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

windowOnload :: Window -> IO t -> MomentIO (Event t)
windowOnload window mk = do
    (ev, fire) <- newEvent
    liftIO $ on window Window.load $ do
        x <- liftIO mk
        liftIO (fire x)
    return ev
