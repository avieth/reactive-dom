{-|
Module      : 
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.ClickToEdit where

import Data.Algebraic.Index
import Data.Algebraic.Product
import Data.Algebraic.Sum
import Reactive.Sequence
import Reactive.DOM.Component
import Reactive.DOM.Component.Label
import Reactive.DOM.Component.TextInput
import GHCJS.Types (JSString)
import GHCJS.DOM.MouseEvent (MouseEvent)
import GHCJS.DOM.Element as Element 

type LabelWithClick = WithEvent MouseEvent () Label

labelWithClick :: LabelWithClick
labelWithClick = WithEvent (Element.click) (\_ _ -> pure ()) label

type ClickToEdit = Composite JSString
                             (Switched (ComponentSum (   TextInput
                                                     :*: LabelWithClick
                                                     )
                                       )
                             )
                             ()

clickToEdit :: ClickToEdit
clickToEdit = Composite makeInput (Switched transition (ComponentSum (textInput .*. labelWithClick))) makeOutput
  where
    makeInput :: JSString -> (ComponentInput TextInput :+: ComponentInput Label)
    makeInput = inject one . always
    transition :: (ComponentOutput TextInput :+: ComponentOutput LabelWithClick)
               -> SEvent (ComponentInput TextInput :+: ComponentInput LabelWithClick)
    transition (Sum sum) = case sum of
        -- We inject whatever data is given on change into the second
        -- component, thereby switching to the label.
        Left (_, changeEvent) -> inject two <$> changeEvent
        Right (clickEvent, currentText) -> const (inject one (always currentText)) <$> clickEvent
    makeOutput = const ()
