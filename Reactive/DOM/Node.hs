{-|
Module      : Reactive.DOM.Node
Description : DOM nodes, exported from the internal module.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

module Reactive.DOM.Node (

      render
    , unrender
    , ElementBuilder
    , liftMomentIO
    , Widget
    , widget
    , knot
    , tie
    , dimap'
    , lmap'
    , rmap'
    , UI
    , ui
    , liftUI
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
    , style
    , styleHover
    , attributes
    , properties
    , IOEvent
    , ioEvent
    , clientHeight
    , clientWidth
    , scrollHeight
    , scrollWidth
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
    , Properties
    , Attributes
    , Tag

    , ChildrenMutation

    , module Reactive.DOM.Internal.ChildrenContainer
    -- , module Reactive.DOM.Children.Cardinality

    ) where

import Reactive.DOM.Internal.Node
import Reactive.DOM.Internal.Mutation
import Reactive.DOM.Internal.ChildrenContainer
import Reactive.DOM.Children.Static
import Reactive.DOM.Children.Single

-- | For any UI, we can "lift it" to a Widget, essentially burying that complete
--   UI one level down in the DOM tree. Hm, sounds counterintuitive: lifting
--   a UI burys it. Maybe come up with better terminology?
liftUI :: UI t -> Widget () t
liftUI ui = widget $ \(_, viewChildren) -> do
    let Static (Single t) = viewChildrenInitial viewChildren
    pure (childData t, constantChildren (Static (Single (newChild ui))))
