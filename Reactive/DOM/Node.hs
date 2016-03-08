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
    , ClosedWidget(..)
    , UI
    , ui
    , widget
    , emptyWidget
    , openWidget
    , closeWidget
    , passthrough
    , knot
    , tie
    , tieKnot
    , dimap'
    , lmap'
    , rmap'
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

    , Element
    , Document
    , ElementSchemaChild
    , makeStyle
    , makeProperties
    , makeAttributes
    , Action(..)
    , Style
    , style
    , styleHover
    , Properties
    , properties
    , Attributes
    , attributes
    , Postprocess(..)
    , postprocess

    , ChildrenMutation

    , module Reactive.DOM.Internal.ChildrenContainer
    -- , module Reactive.DOM.Children.Cardinality

    ) where

import Reactive.DOM.Internal.Node
import Reactive.DOM.Internal.Tag
import Reactive.DOM.Internal.Mutation
import Reactive.DOM.Internal.ChildrenContainer
import Reactive.DOM.Children.Static
import Reactive.DOM.Children.Single
import Reactive.DOM.Children.NodeList
import Data.Profunctor

-- | Make any ClosedWidget into an OpenWidget. The OpenWidget has that
--   ClosedWidget as its only child, always.
openWidget :: forall s t . ClosedWidget s t -> OpenWidget s t
openWidget (ClosedWidget _ w) = widget $ \(~(s, viewChildren)) -> do
    let child :: UI t
        child = ui (lmap (const s) w)
    let Static (Single t) = viewChildrenInitial viewChildren
    pure (childData t, constantChildren (Static (Single (newChild child))))

emptyWidget :: OpenWidget s ()
emptyWidget = widget $ \_ -> pure ((), constantChildren (Static (nodeList [])))
