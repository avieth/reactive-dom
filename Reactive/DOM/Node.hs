{-|
Module      : Reactive.DOM.Node
Description : DOM nodes, exported from the internal module.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Reactive.DOM.Node (

      render
    , unrender
    , ElementBuilder
    , liftMomentIO
    , Tag(..)
    , W3CTag
    , Widget
    , OpenWidget
    -- TBD export constructor?
    , ClosedWidget(..)
    , UI
    , ui
    , widget
    , pullthrough
    , trivialWidget
    , emptyWidget
    , openWidget
    , closeWidget
    , tieKnot
    , tieKnot'
    , Children
    , constantChildren
    , children
    , ViewChildren
    , viewChildrenBehavior
    , viewChildrenInitial
    , viewChildrenChanges
    , viewChildrenEvent
    , viewChildrenTrans
    , Child
    , childData
    , SetChild
    , newChild
    , existingChild
    , textChild
    , IOEvent
    , ioEvent
    , clientHeight
    , clientWidth
    , scrollHeight
    , scrollWidth
    , ElementEvent
    , event
    , Click(..)
    , Submit(..)
    , Input(..)
    , Mouseenter(..)
    , Mouseleave(..)
    , Scroll(..)
    , ScrollData(..)
    , Modifier
    , modifier
    , modify
    , modify_
    , modifyr
    , modifyl

    , Element
    , Document
    , ElementSchemaChild
    , makeStyle
    , makeProperties
    , makeAttributes
    , Action(..)
    , Style
    , style
    , style'
    , styleHover
    , Properties
    , properties
    , properties'
    , Attributes
    , attributes
    , attributes'
    , Postprocess(..)
    , postprocess

    , ChildrenMutation

    , module Reactive.DOM.Internal.ChildrenContainer
    -- , module Reactive.DOM.Children.Cardinality

    , touchPull

    ) where

import Reactive.DOM.Internal.Node
import Reactive.DOM.Internal.Tag
import Reactive.DOM.Internal.Mutation
import Reactive.DOM.Internal.ChildrenContainer
import Reactive.DOM.Children.Static
import Reactive.DOM.Children.Single
import Reactive.DOM.Children.NodeList
import Data.Profunctor
import Data.Void

-- | Make any ClosedWidget into an OpenWidget. The OpenWidget has that
--   ClosedWidget as its only child, always.
openWidget :: forall s t . ClosedWidget s t -> OpenWidget s t
openWidget (ClosedWidget _ w) = widget $ \(~(s, viewChildren)) -> do
    let child :: UI t
        child = ui (lmap (const s) w)
    let Static (Single t) = viewChildrenInitial viewChildren
    pure (childData t, constantChildren (Static (Single (newChild child))))

trivialWidget :: OpenWidget () ()
trivialWidget = widget $ \_ -> pure ((), constantChildren (Static (nodeList [])))

emptyWidget :: OpenWidget Void Void
emptyWidget = widget mk
  where
    mk :: forall tag . (Void, ViewChildren (NodeList Void Void Void)) -> ElementBuilder tag (Void, Children (NodeList Void Void Void))
    mk = absurd . fst
