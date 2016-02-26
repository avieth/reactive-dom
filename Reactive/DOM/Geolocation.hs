{-|
Module      : 
Description : 
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.DOM.Geolocation where

import Control.Concurrent (forkIO)
import Control.Exception (try)
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import GHCJS.DOM.Window as Window
import GHCJS.DOM.Navigator as Navigator
import GHCJS.DOM.Geolocation as Geolocation
import GHCJS.DOM.Geoposition as Geoposition
import GHCJS.DOM.Coordinates as Coordinates
import GHCJS.DOM.PositionError (PositionException)

geolocation :: Window -> MomentIO (Event (Maybe (Double, Double)))
geolocation window = do
    Just navigator <- Window.getNavigator window
    Just geolocation <- Navigator.getGeolocation navigator
    (position, firePosition) <- newEvent
    liftIO $ forkIO $ do
        Prelude.print "Attempting to get geoposition"
        egeoposition :: Either PositionException Geoposition
            <- try (Geolocation.getCurrentPosition geolocation Nothing)
        case egeoposition of
            Left exception -> do
                Prelude.print "Failed to get geoposition"
                firePosition Nothing
            Right geoposition -> do
                Prelude.print "Got geoposition"
                mcoords <- Geoposition.getCoords geoposition
                case mcoords of
                    Nothing -> firePosition Nothing
                    Just coords -> do
                        lat <- Coordinates.getLatitude coords
                        lon <- Coordinates.getLongitude coords
                        firePosition (Just (lat, lon))
    pure position
