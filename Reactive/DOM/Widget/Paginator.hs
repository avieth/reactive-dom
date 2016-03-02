{-|
Module      : Reactive.DOM.Widget.Paginator
Description : Infrastructure for UIs which do pagination of other UIs.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Reactive.DOM.Widget.Paginator where

import Prelude hiding ((.), id, span)
import Control.Category
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.DOM.Node
import Reactive.DOM.Widget.Primitive
import Reactive.DOM.Widget.MonotoneList
import Reactive.DOM.Children.Algebraic hiding (Sum(..))
import Reactive.DOM.Children.NodeList
import Reactive.Sequence
import Data.Profunctor
import Data.Monoid

data PaginatorInput t = More [UI t] | Exhausted [UI t] | Expect

data PaginatorState = Incomplete | Complete | Fetching
  deriving (Show)

data PaginatorOutput = Fetch Int
  deriving (Show)

paginatorInputUIs :: PaginatorInput t -> [UI t]
paginatorInputUIs x = case x of
    More uis -> uis
    Exhausted uis -> uis
    Expect -> []

prependIncrement :: t -> (Sum Int, t)
prependIncrement t = (Sum 1, t)

-- | A widget for displaying a paginated list of user interfaces.
--
--   You tell it precisely what the input is, and it tells you whenever the
--   user asks for more, including a 0-indexed offset (i.e. the number of
--   UIs it's displaying in the monotone list).
--
--   It is *not* concerned with the provenance of the event which produces
--   the children UIs, even though it's almost certainly going to be
--   derived from the output event of the paginator. But that's up to the
--   user of the paginator to define.
paginator
    :: forall t .
       ( Monoid t )
    => Widget (PaginatorInput t, Event (PaginatorInput t))
              ((t, Event t), Event PaginatorOutput)
paginator = lmap makeInput (rmap' makeOutput product)
  where

    paginatorState
        :: (PaginatorInput t, Event (PaginatorInput t))
        -> (PaginatorState, Event PaginatorState)
    paginatorState (x, y) = (makePaginatorState x, fmap makePaginatorState y)

    makePaginatorState :: PaginatorInput t -> PaginatorState
    makePaginatorState x = case x of
        More _ -> Incomplete
        Exhausted _ -> Complete
        Expect -> Fetching

    loadingButton :: Widget (PaginatorState, Event PaginatorState) (Event ())
    loadingButton = widget $ \((initialState, evState), viewChildren) -> do
        let getClickEvent :: NodeList (Event ()) Child -> Event ()
            getClickEvent (NodeList []) = never
            getClickEvent (NodeList (x : _)) = childData x
        let childrenSequence = viewChildrenInitial viewChildren |> viewChildrenEvent viewChildren
        let clickEventSequence = getClickEvent <$> childrenSequence
        click <- sequenceSwitchE clickEventSequence
        let makeChildren :: PaginatorState -> NodeList (Event ()) SetChild
            makeChildren x = case x of
                Incomplete -> nodeList [newChild button]
                Fetching -> nodeList [newChild loading]
                Complete -> nodeList []
        let firstChildren = makeChildren initialState
        let evChildren = pure . makeChildren <$> evState
        pure (click, children firstChildren evChildren)

    -- At its core the paginator is a monotone list widget followed by a
    -- widget which is either a button to fetch more, a loading indicator, or
    -- nothing (in case the paginator is exhausted).
    -- That second component is a function of a PaginatorState.
    product :: Widget (([UI (Sum Int, t)], Event [UI (Sum Int, t)]), (PaginatorState, Event PaginatorState))
                      (((Sum Int, t), Event (Sum Int, t)), Event ())
    product = monotoneListWidget `widgetProduct` loadingButton

    -- How to come up with the current offset?
    -- We make (Sum Int, t), which is also a monoid.
    -- Then we get out ((Sum Int, t), Event (Sum Int, t)), take a behavior of
    -- the first and use it against the button's click event.

    makeInput
        :: (PaginatorInput t, Event (PaginatorInput t))
        -> (([UI (Sum Int, t)], Event [UI (Sum Int, t)]), (PaginatorState, Event PaginatorState))
    -- Lazy match is very important here.
    makeInput (~(initialInput, evInput)) =
        let state = paginatorState (initialInput, evInput)
            initialUIs = (fmap . fmap) prependIncrement . paginatorInputUIs $ initialInput
            evUIs = (fmap . fmap) prependIncrement . paginatorInputUIs <$> evInput
        in  ((initialUIs, evUIs), state)

    makeOutput
        :: (((Sum Int, t), Event (Sum Int, t)), Event ())
        -> ElementBuilder ((t, Event t), Event PaginatorOutput)
    makeOutput (~((x1, y1), click)) = do
        let initialT = snd x1
        let evT = snd <$> y1
        let initialSum = fst x1
        let evSum = fst <$> y1
        total <- stepper initialSum evSum
        let output = (Fetch . getSum <$> total) <@ click
        pure ((initialT, evT), output)

    button :: UI (Event ())
    button = span (rmap' (const (event Click)) (constantText "Get more"))

    loading :: UI (Event ())
    loading = span (rmap' (const (pure never)) (constantText "Loading..."))
