{-|
Module      : Reactive.DOM.Widget.MonotoneList
Description : Definition of a Widget which shows a monotone list of Widgets
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Reactive.DOM.Widget.MonotoneList where

import Data.Monoid
import Reactive.Sequence
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.DOM.Node
import Reactive.DOM.Children.MonotoneList

-- | The input sequence is interpreted as follows:
--   Whenever a new list appears, its widgets are appended after the current
--   list of children.
--   The values carried by those widgets are accumulated such that whenever the
--   input sequence changes, so does the output sequence, with the new
--   monoidal concatenation of all widgets so far observed.
--
--   Input/output is a tuple rather than a sequence because if it were a
--   sequence then we'd have to force it. If runSequence were lazy then we'd be
--   ok. Could we somehow make it lazy?
monotoneListWidget
    :: forall t .
       ( Monoid t )
    => Widget ([UI t], Event [UI t]) (t, Event t)
monotoneListWidget = widget $ \(~((initial, changes), viewChildren :: ViewChildren (MonotoneList t))) -> mdo

    -- Use the ViewChildren to come up with the output sequence.
    let concatOne :: MonotoneList t Child -> t
        concatOne = mconcat . fmap childData . runMonotoneList
    let concatMany :: [MonotoneList t Child] -> t
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
