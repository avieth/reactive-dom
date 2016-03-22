{-|
Module      : Reactive.DOM.Widget.Picture
Description : Tools for making picture widgets.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Reactive.DOM.Widget.Picture (

      PictureSource(..)
    , DataUri(..)
    , picture
    , divPicture

    ) where

import qualified Data.Text as T
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Children.Static
import Reactive.DOM.Children.NodeList
import Debug.Trace

data PictureSource = PictureSourceDataUri DataUri

data DataUri = DataUri {
      dataUriMimeType :: T.Text
    , dataUriCharset :: Maybe T.Text
    , dataUriBase64 :: Bool
    , dataUriPayload :: T.Text
    }

dataUri :: DataUri -> T.Text
dataUri duri = T.concat [
      "data:"
    , dataUriMimeType duri
    , maybe "" (T.cons ';') (dataUriCharset duri)
    , if dataUriBase64 duri then ";base64" else ""
    , ","
    , dataUriPayload duri
    ]

-- | A static picture.
picture :: Widget "img" PictureSource ()
picture = trivialWidget `modifyl` modifier setup
  where
    setup psource = attributes (always (mkAttributes psource))
    mkAttributes (PictureSourceDataUri duri) = Set (makeAttributes [("src", dataUri duri)])

-- | Like picture, but a div with a background image rather than an img element.
divPicture :: Widget "div" PictureSource ()
divPicture = trivialWidget `modifyl` modifier setup
  where
    setup psource = style (always (mkStyle psource))
    mkStyle (PictureSourceDataUri duri) = Set $ makeStyle [
          ("background-image", mconcat ["url(", dataUri duri, ")"])
        ]
