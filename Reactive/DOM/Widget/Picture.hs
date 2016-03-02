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

module Reactive.DOM.Widget.Picture (

      PictureSource(..)
    , DataUri(..)
    , picture

    ) where

import qualified Data.Text as T
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Children.Static
import Reactive.DOM.Children.NodeList

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
picture :: PictureSource -> Widget ()
picture sdatauri = widget $ \_ -> do
    tag "img"
    attributes attrs
    pure ((), constantChildren (Static (nodeList [])))
  where
    mkAttributes (PictureSourceDataUri duri) = Set (makeAttributes [("src", dataUri duri)])
    attrs = always (mkAttributes sdatauri)
