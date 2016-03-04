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
    , Widget
    , OpenWidget
    , widget
    , openWidget
    , closeWidget
    , passthrough
    , knot
    , tie
    , dimap'
    , lmap'
    , rmap'
    , UI
    , ui
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
import Data.Profunctor

-- | Make any Widget into an OpenWidget. The OpenWidget has that Widget as its
--   only child, always.
openWidget :: forall tag s t . W3CTag tag => Widget tag s t -> OpenWidget s t
openWidget w = widget $ \(~(s, viewChildren)) -> do
    let child :: UI t
        child = ui (lmap (const s) w)
    let Static (Single t) = viewChildrenInitial viewChildren
    pure (childData t, constantChildren (Static (Single (newChild child))))
