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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Reactive.DOM.Widget.Paginator where

import Prelude hiding ((.), id, span)
import Control.Category
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.DOM.Node
import Reactive.DOM.Widget.Common
import Reactive.DOM.Widget.List
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
    => OpenWidget (Sequence (PaginatorInput t))
                  (Sequence t, Event PaginatorOutput)
paginator = lmap makeInput (rmap' makeOutput product)
  where

    makePaginatorState :: PaginatorInput t -> PaginatorState
    makePaginatorState x = case x of
        More _ -> Incomplete
        Exhausted _ -> Complete
        Expect -> Fetching

    loadingButton :: OpenWidget (Sequence PaginatorState) (Event ())
    loadingButton = widget $ \(seqnc, viewChildren) -> do
        let getClickEvent :: forall inp out . NodeList (Event ()) inp out Child -> Event ()
            getClickEvent (NodeList []) = never
            getClickEvent (NodeList (x : _)) = childData x
        let childrenSequence = viewChildrenInitial viewChildren |> viewChildrenEvent viewChildren
        let clickEventSequence = getClickEvent <$> childrenSequence
        click <- liftMoment (sequenceSwitchE clickEventSequence)
        let makeChildren :: forall inp out . PaginatorState -> NodeList (Event ()) inp out SetChild
            makeChildren x = case x of
                Incomplete -> nodeList [newChild button]
                Fetching -> nodeList [newChild loading]
                Complete -> nodeList []
        (initialState, evState) <- liftMoment $ runSequence seqnc
        let firstChildren = makeChildren initialState
        let evChildren = pure . makeChildren <$> evState
        pure (click, children firstChildren evChildren)

    -- At its core the paginator is a monotone list widget followed by a
    -- widget which is either a button to fetch more, a loading indicator, or
    -- nothing (in case the paginator is exhausted).
    -- That second component is a function of a PaginatorState.
    product
        :: OpenWidget (Sequence [UI (Sum Int, t)], Sequence PaginatorState)
                      (Sequence (Sum Int, t), Event ())
    product = monotoneListWidget `widgetProduct` loadingButton

    -- How to come up with the current offset?
    -- We make (Sum Int, t), which is also a monoid.
    -- Then we get out ((Sum Int, t), Event (Sum Int, t)), take a behavior of
    -- the first and use it against the button's click event.

    makeInput
        :: (Sequence (PaginatorInput t))
        -> (Sequence [UI (Sum Int, t)], Sequence PaginatorState) 
    makeInput seqnc = 
        let uis = (fmap . fmap) prependIncrement . paginatorInputUIs <$> seqnc
            state = makePaginatorState <$> seqnc
        in  (uis, state)

    makeOutput
        :: (Sequence (Sum Int, t), Event ())
        -> ElementBuilder tag (Sequence t, Event PaginatorOutput)
    makeOutput (seqnc, click) = do
        total <- behavior (fst <$> seqnc)
        let output = (Fetch . getSum <$> total) <@ click
        pure (snd <$> seqnc, output)

    button :: UI (Event ())
    button = let open = lmap (const "Get more") (span constantText)
                 closed = rmap' (const (event Click)) open
             in  ui closed

    loading :: UI (Event ())
    loading = let open = lmap (const "Loading...") (span constantText)
                  closed = rmap (const never) open
              in  ui closed
