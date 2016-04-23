{-|
Module      : Reactive.DOM.Flow.Directed
Description : Definition of directed widget flows.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Reactive.DOM.Flow.Directed where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow.Flow
import Data.Functor.Compose
import Data.Union
import Data.Transition
import Reactive.DOM.Flow
import Reactive.Banana.Combinators
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Internal.Mutation

-- | Run a flow using the Direction functor. New flow items will animate in
--   and out of view. It won't look right unless the widget is given good
--   width and height bounds, such that it does not grow to accomodate new
--   children.
runDirectedWidgetFlow
    :: forall tag fixed t .
       ( W3CTag tag )
    => UI (Event (Union fixed (Transition Direction (FlowContinuation (WidgetN fixed Direction) t))))
    -> Widget tag () (Event (Union fixed (Transition Direction t)))
runDirectedWidgetFlow =

    runGeneralWidgetFlow getChild setChildI setChildC setParentStyle

  where

    getChild :: forall f s t r . DirectedContainer t s (Event r) f -> f t
    getChild (DirectedContainer (_, x, _)) = x

    setChildI
        :: forall s t r .
           UI (Event (Union fixed (Transition Direction t)))
        -> DirectedContainer (Event (Union fixed (Transition Direction t))) s (Event r) SetChild
    setChildI x = DirectedContainer (North, newChild (setChildStyle North x), Nothing)

    setChildC
        :: forall s t r .
           Transition Direction (UI (Event (Union fixed (Transition Direction t))))
        -> [Change (DirectedContainer (Event (Union fixed (Transition Direction t))) s (Event r)) SetChild]
    setChildC (Transition (d, x)) = [DirectedChange (d, newChild (setChildStyle d x))]

    parentStyleBase :: Style
    parentStyleBase = makeStyle [
          ("width", "100%")
        , ("height", "100%")
        , ("display", "flex")
        , ("overflow", "hidden")
        ]

    parentStyle :: Direction -> Style
    parentStyle d = makeStyle [
          if isHorizontal d then ("flex-direction", "row") else ("flex-direction", "column")
        ]

    setParentStyle :: forall t . Event (Transition Direction t) -> ElementBuilder tag ()
    setParentStyle ev = do
        style (always (Set parentStyleBase))
        style (Set . parentStyle . takeTransition <$> (Transition (North, undefined) |> ev))

    setChildStyle
        :: forall t .
           Direction
        -> UI (Event (Union fixed (Transition Direction t)))
        -> UI (Event (Union fixed (Transition Direction t)))
    setChildStyle d (ClosedWidget tag w) = ClosedWidget tag (w `modifyr` childModifier d)

    childModifier
        :: forall t tag .
           ( W3CTag tag )
        => Direction
        -> Modifier tag (Event (Union fixed (Transition Direction t))) (Event (Union fixed (Transition Direction t)))
    childModifier d = modifier (makeTransition d)

    makeTransition
        :: forall t tag .
           ( W3CTag tag )
        => Direction
        -> Event (Union fixed (Transition Direction t))
        -> ElementBuilder tag (Event (Union fixed (Transition Direction t)))
    makeTransition d ev = do
        let ev' = eright ev
        animFrame <- requestAnimationFrame
        let styleOn = const (Set (onStyle d)) <$> animFrame
        let styleOff = Set . offStyle . takeTransition <$> ev'
        control <- stepper False (const True <$> ev')
        let styleChange = unionWith const styleOff (whenE (not <$> control) styleOn)
        style (Set (initialStyle d) |> styleChange)
        pure ev
    
    initialStyle :: forall t . Direction -> Style
    initialStyle d = makeStyle [
          ("transition", "all 1s")
        , ("-webkit-transition", "all 1s")
        , if isHorizontal d then ("width", "0px") else ("height", "0px")
        , ("opacity", "0")
        ]

    onStyle :: forall t . Direction -> Style
    onStyle d = makeStyle [
          ("transition", "all 1s")
        , ("-webkit-transition", "all 1s")
        , ("width", "100%")
        , ("height", "100%")
        , ("opacity", "1")
        ]

    offStyle :: forall t . Direction -> Style
    offStyle d = makeStyle [
          ("transition", "all 1s")
        , ("-webkit-transition", "all 1s")
        , if isHorizontal d then ("width", "0px") else ("height", "0px")
        , ("opacity", "0")
        ]

type DirectedTransition = Transition Direction

north :: t -> DirectedTransition t
north = Transition . (,) North

south :: t -> DirectedTransition t
south = Transition . (,) South

east :: t -> DirectedTransition t
east = Transition . (,) East

west :: t -> DirectedTransition t
west = Transition . (,) West

data Direction =
      North
    | South
    | East
    | West

isHorizontal :: Direction -> Bool
isHorizontal West = True
isHorizontal East = True
isHorizontal _ = False

isVertical :: Direction -> Bool
isVertical = not . isHorizontal

newtype DirectedContainer r s t f = DirectedContainer {
      runDirectedContainer :: (Direction, f r, Maybe (f r))
    }

newtype DirectedChange r s t f = DirectedChange {
      runDirectedChange :: (Direction, f r)
    }

instance FunctorTransformer (DirectedContainer r s t) where
    functorTrans f (DirectedContainer (d, x, my)) = DirectedContainer (d, f x, fmap f my)
    functorCommute (DirectedContainer (d, mx, mmy)) =
        let x = getCompose mx
            my = sequenceA (getCompose <$> mmy)
        in  DirectedContainer <$> ((,,) <$> pure d <*> x <*> my)

instance FunctorTransformer (DirectedChange r s t) where
    functorTrans f (DirectedChange (d, fx)) = DirectedChange (d, f fx)
    functorCommute (DirectedChange (d, fx)) =
        let x = getCompose fx
        in  DirectedChange <$> ((,) <$> pure d <*> x)

instance ChildrenContainer (DirectedContainer r s t) where

    type Change (DirectedContainer r s t) = DirectedChange r s t

    getChange get (DirectedChange (dnew, it)) (DirectedContainer (dold, x, my)) =
        let removals = case my of
                Nothing -> []
                Just old -> [RemoveChild (get old)]
            additions = case dnew of
                North -> [AppendChild (get it)]
                South -> [InsertBefore (get it) (get x)]
                East -> [AppendChild (get it)]
                West -> [InsertBefore (get it) (get x)]
        in  (DirectedContainer (dnew, it, Just x), additions ++ removals)

    childrenContainerList get (DirectedContainer (d, it, my)) =
        let other = case my of
                Nothing -> []
                Just y -> [get y]
        in  case d of
                North -> other ++ [get it]
                South -> [get it] ++ other
                East -> other ++ [get it]
                West -> [get it] ++ other
