{-|
Module      : Reactive.DOM.Widget.List
Description : Widgets to show lists of UIs.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Reactive.DOM.Widget.List where

import Data.Monoid
import Reactive.Sequence
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.DOM.Node
import Reactive.DOM.Children.NodeList
import Reactive.DOM.Children.MonotoneList

-- | Show a list of UIs in the list order, and give back the monoidal
--   concatenation of their values.
--
--   Input/output is a tuple rather than a sequence because if it were a
--   sequence then we'd have to force it. If runSequence were lazy then we'd be
--   ok. Could we somehow make it lazy?
nodeListWidget
    :: forall m t .
       ( Monoid t )
    => OpenWidget ([UI t], Event [UI t]) (t, Event t)
nodeListWidget = widget $ \(~((initial, rest), viewChildren)) -> do

    -- Values are derived from the viewChildren.
    let concatOne :: forall inp out . NodeList t inp out Child -> t
        concatOne = mconcat . fmap childData . runNodeList
    let firstT :: t
        firstT = concatOne $ viewChildrenInitial viewChildren
    let restT :: Event t
        restT = concatOne <$> viewChildrenEvent viewChildren
    let seqncT = (firstT, restT)

    -- Children are derived easily from the input.
    let firstChildren = nodeList . fmap newChild $ initial
    let restChildren = pure . nodeList . fmap newChild <$> rest
    let setChildren = children firstChildren restChildren

    pure (seqncT, setChildren)

-- | Like nodeListWidget except using a MonotoneList. It can be more efficient:
--   the monoidal product is computed differentially: whenever the children
--   change, we don't have to run over the entire new set of children, we just
--   apply the differences.
--
--   The input sequence is interpreted as follows:
--   Whenever a new list appears, its widgets are appended after the current
--   list of children.
--   The values carried by those widgets are accumulated such that whenever the
--   input sequence changes, so does the output sequence, with the new
--   monoidal concatenation of all widgets so far observed.
monotoneListWidget
    :: forall t .
       ( Monoid t )
    => OpenWidget ([UI t], Event [UI t]) (t, Event t)
monotoneListWidget = widget $ \(~((initial, changes), viewChildren)) -> mdo

    -- Use the ViewChildren to come up with the output sequence.
    let concatOne :: forall inp out . MonotoneList t inp out Child -> t
        concatOne = mconcat . fmap childData . runMonotoneList
    let concatMany :: forall inp out . [MonotoneList t inp out Child] -> t
        concatMany = mconcat . fmap concatOne
    let firstT :: t
        firstT = concatOne $ viewChildrenInitial viewChildren
    let changeT :: Event t
        changeT = concatMany <$> viewChildrenChanges viewChildren
    let accumulatedT :: Event t
        accumulatedT = (<>) <$> currentT <@> changeT
    currentT :: Behavior t
        <- stepper firstT accumulatedT

    let childrenInitial = MonotoneList . fmap newChild $ initial
    let childrenChanges = MonotoneList . fmap newChild <$> changes

    pure ((firstT, accumulatedT), children childrenInitial (pure <$> childrenChanges))
