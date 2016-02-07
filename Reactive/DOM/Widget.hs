{-|
Module      : Reactive.DOM.Widget
Description : Definition of Widgets.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.DOM.Widget where

import Control.Applicative
import Data.Traversable
import Data.Monoid (mempty, mconcat)
import qualified Data.Map as M
import Data.Functor.Compose
import Data.Semigroup (First(..), (<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Identity
import Reactive.DOM.Node
import Reactive.Sequence
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import GHCJS.DOM
import GHCJS.DOM.Window
import GHCJS.DOM.Document
import GHCJS.DOM.Element as Element (click, input, mouseEnter, mouseLeave, submit)
import GHCJS.DOM.HTMLInputElement (castToHTMLInputElement, getValue)
import GHCJS.DOM.Event (preventDefault)

-- | A thing which can be displayed, and which produces some output when it
--   is run. This parameter allows data from descendant Widgets to be routed
--   up through ancestral ones.
--
--   There's very little type information about such a thing.
--   Nothing at the type level describes look and feel, or what the thing
--   is supposed to represent.
data Widget t = Widget {
      runWidget :: (t, VirtualElement)
    }

instance Functor Widget where
    fmap f = Widget . (\(x, y) -> (f x, y)) . runWidget

type WidgetConstructor t = MomentIO (Widget t)

-- | Kind of like monadic bind.
withValue
    :: WidgetConstructor t
    -> (t -> MomentIO r)
    -> WidgetConstructor r
withValue wk k = do
    (t, velem) <- runWidget <$> wk
    r <- k t
    pure $ Widget (r, velem)

-- | Define a WidgetConstructor recursively. Just pass your recursive arguments
--   through the second parameter of the resulting Widget.
knot :: (r -> WidgetConstructor (t, r)) -> WidgetConstructor t
knot k = mdo
    w <- k r
    let ((t, r), velem) = runWidget w
    pure $ Widget (t, velem)

-- | Thin wrapper over VirtualElement. From here all Widgets are derived.
element_
    :: Tag
    -> Sequence [Properties]
    -> Sequence [Attributes]
    -> Sequence [Style]
    -> Sequence [VirtualNode]
    -> WidgetConstructor ()
element_ tag props attrs style children = do
    velem <- virtualElement tag props attrs style children
    pure $ Widget ((), velem)

-- | Create a Widget whose properties, attributes, style, and children
--   are determined by sequences.
element
    :: forall t .
       Tag
    -> Sequence [Properties]
    -> Sequence [Attributes]
    -> Sequence [Style]
    -> Sequence [WidgetConstructor t]
    -> WidgetConstructor (Sequence [t])
element tag props attrs style children = do
    let sequenced :: Sequence (MomentIO [Widget t])
        sequenced = sequence <$> children
    commuted :: Sequence [Widget t]
        <- sequenceCommute sequenced
    let widgets :: Sequence ([t], [VirtualElement])
        widgets = unzip . fmap runWidget <$> commuted
    let outs :: Sequence [t]
        outs = fst <$> widgets
    let velems :: Sequence [VirtualNode]
        velems = fmap node . snd <$> widgets
    let el = element_ tag props attrs style velems
    (fmap . fmap) (const outs) el

-- | Widgets can be treated applicatively by injecting them into this type.
--   When returned to Widget via runApplicativeWidget, the resulting Widget
--   contains all component Widgets in one element, in their syntactic order.
data ApplicativeWidget t where
    ApplicativeWidgetPure :: t -> ApplicativeWidget t
    ApplicativeWidgetImpure :: WidgetConstructor t -> ApplicativeWidget t
    ApplicativeWidgetAp
        :: ApplicativeWidget (s -> t)
        -> ApplicativeWidget s
        -> ApplicativeWidget t

instance Functor ApplicativeWidget where
    fmap f apw = case apw of
        ApplicativeWidgetPure x -> ApplicativeWidgetPure (f x)
        ApplicativeWidgetImpure w -> ApplicativeWidgetImpure ((fmap . fmap) f w)
        ApplicativeWidgetAp mf mx -> ApplicativeWidgetAp ((fmap . fmap) f mf) mx

instance Applicative ApplicativeWidget where
    pure = ApplicativeWidgetPure
    (<*>) = ApplicativeWidgetAp

applicativeWidget :: WidgetConstructor t -> ApplicativeWidget t
applicativeWidget = ApplicativeWidgetImpure

runApplicativeWidget' :: ApplicativeWidget t -> MomentIO (t, [VirtualElement])
runApplicativeWidget' apw = case apw of
    ApplicativeWidgetPure t -> pure (t, [])
    ApplicativeWidgetImpure wk -> do
        (t, velem) <- runWidget <$> wk
        pure (t, [velem])
    ApplicativeWidgetAp mf mx -> do
        (f, velemsf) <- runApplicativeWidget' mf
        (x, velemsx) <- runApplicativeWidget' mx
        pure (f x, velemsf ++ velemsx)

runApplicativeWidget
    :: Tag
    -> Sequence [Properties]
    -> Sequence [Attributes]
    -> Sequence [Style]
    -> ApplicativeWidget t
    -> WidgetConstructor t
runApplicativeWidget tag props attrs style apw = do
    (t, velemList) <- runApplicativeWidget' apw
    let children = always (node <$> velemList)
    (fmap . fmap) (const t) (element_ tag props attrs style children)

styleWidget :: Sequence (Action Style) -> WidgetConstructor t -> WidgetConstructor t
styleWidget actions widgetk = do
    (t, velem) <- runWidget <$> widgetk
    let styledVelem = styleVirtualElement actions velem
    pure $ Widget (t, styledVelem)

-- | Convenience function to set a style on hover (set on mouse enter and
--   unset on mouse leave).
styleHover :: Style -> WidgetConstructor t -> WidgetConstructor t
styleHover styl widgetk = mdo
    let styleSet :: Sequence (Action Style)
        styleSet = NoOp |> (const (Set styl) <$> enter)
    let styleUnset :: Sequence (Action Style)
        styleUnset = NoOp |> (const (Unset styl) <$> leave)
    let styleCombined = getFirst <$> ((First <$> styleSet) <> (First <$> styleUnset))
    let styledk = styleWidget styleCombined widgetk
    styled <- styledk
    enter <- widgetEvent Mouseenter styled
    leave <- widgetEvent Mouseleave styled
    pure styled


attributesWidget :: Sequence (Action Attributes) -> WidgetConstructor t -> WidgetConstructor t
attributesWidget actions widgetk = do
    (t, velem) <- runWidget <$> widgetk
    let attributesVelem = attributesVirtualElement actions velem
    pure $ Widget (t, attributesVelem)

propertiesWidget :: Sequence (Action Properties) -> WidgetConstructor t -> WidgetConstructor t
propertiesWidget actions widgetk = do
    (t, velem) <- runWidget <$> widgetk
    let propertiesVelem = propertiesVirtualElement actions velem
    pure $ Widget (t, propertiesVelem)

class WidgetEvent e where
    type EventData e :: *
    getWidgetEvent :: e -> VirtualElement -> MomentIO (Event (EventData e))

data Click = Click
instance WidgetEvent Click where
    type EventData Click = ()
    getWidgetEvent Click velem =
        virtualEvent velem Element.click (\_ _ -> pure ())

data Mouseenter = Mouseenter
instance WidgetEvent Mouseenter where
    type EventData Mouseenter = ()
    getWidgetEvent Mouseenter velem =
        virtualEvent velem Element.mouseEnter (\_ _ -> pure ())

data Mouseleave = Mouseleave
instance WidgetEvent Mouseleave where
    type EventData Mouseleave = ()
    getWidgetEvent Mouseleave velem =
        virtualEvent velem Element.mouseLeave (\_ _ -> pure ())

data Submit = Submit
instance WidgetEvent Submit where
    type EventData Submit = ()
    getWidgetEvent Submit velem =
        virtualEvent velem Element.submit getData
      where
        -- We always prevent default, else the page will reload.
        getData el ev = preventDefault ev >> pure ()

data Input = Input
instance WidgetEvent Input where
    type EventData Input = T.Text
    getWidgetEvent Input velem =
        virtualEvent velem Element.input getData
      where
        getData el _ = maybe "" id <$> getValue (castToHTMLInputElement el)

widgetEvent
    :: WidgetEvent e
    => e
    -> Widget t
    -> MomentIO (Event (EventData e))
widgetEvent e ~w = do
    let (_, velem) = runWidget w
    getWidgetEvent e velem

-- | Factor an Event into a WidgetConstructor.
withEvent
    :: WidgetEvent e
    => e
    -> (Event (EventData e) -> s -> t)
    -> WidgetConstructor s
    -> WidgetConstructor t
withEvent e f wk = do
    w <- wk
    ev <- widgetEvent e w
    pure (f ev <$> w)

label :: Sequence Text -> WidgetConstructor ()
label stext =
    let children = pure . text <$> stext
    in  element_ "span" (always mempty) (always mempty) (always mempty) children

data PictureSource = PictureSourceDataUri DataUri
data DataUri = DataUri {
      dataUriMimeType :: Text
    , dataUriCharset :: Maybe Text
    , dataUriBase64 :: Bool
    , dataUriPayload :: Text
    }

dataUri :: DataUri -> Text
dataUri duri = T.concat [
      "data:"
    , dataUriMimeType duri
    , maybe "" (T.cons ';') (dataUriCharset duri)
    , if dataUriBase64 duri then ";base64" else ""
    , ","
    , dataUriPayload duri
    ]

picture :: Sequence PictureSource -> WidgetConstructor ()
picture sdatauri =
    let mkAttributes :: PictureSource -> [Attributes]
        mkAttributes (PictureSourceDataUri duri) = [makeAttributes [("src", dataUri duri)]]
        attributes = mkAttributes <$> sdatauri
    in  element_ "img" (always mempty) attributes (always mempty) (always [])

-- | A button with a text label.
button :: Sequence T.Text -> WidgetConstructor ()
button stext = runApplicativeWidget "button" (always mempty) (always mempty) (always mempty)
    $ (const ()) <$> applicativeWidget (label stext)
   
textInput :: Sequence Text -> WidgetConstructor ()
textInput stext = mdo
    let attributes = [makeAttributes [("type", "text")]]
    let properties val = [makeProperties [("value", val)]]
    element_ "input" (properties <$> stext) (always attributes) (always mempty) (always [])
    
passwordInput :: Sequence Text -> WidgetConstructor ()
passwordInput = attributesWidget actions . textInput
  where
    actions :: Sequence (Action Attributes)
    actions = always (Set (makeAttributes [("type", "password")]))
