{-|
Module      : Reactive.DOM.Flow
Description : Definition of widget flows.
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

module Reactive.DOM.Flow where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Arrow.Flow
import Data.Profunctor
import Data.Bifunctor
import Data.Union
import Data.Transition
import Reactive.Banana.Combinators
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Children.Single

-- | Symbolic widget flow piece. It's a ClosedWidget taking the input type,
--   and always giving an Event with a transition and some other data.
newtype WidgetM fixed transition s t = WidgetM {
      runWidgetM :: ClosedWidget s (Event (Union fixed (Transition transition t)))
    }

instance Profunctor (WidgetM fixed transition) where
    dimap l r (WidgetM cw) = WidgetM (dimap l ((fmap . fmap . fmap) r) cw)

instance Functor (WidgetM fixed transition s) where
    fmap = rmap

-- | The functor variant of a WidgetM.
--   Any WidgetM fixed transition s t can be transformed into a
--
--     Kleisli (FlowContinuation (WidgetN fixed transition)) s t
--
--   which is an Arrow, because WidgetN fixed transition is a functor!
newtype WidgetN fixed transition t = WidgetN {
      runWidgetN :: UI (Event (Union fixed (Transition transition t)))
    }

instance Functor (WidgetN fixed transition) where
    fmap f = WidgetN . (fmap . fmap . fmap . fmap) f . runWidgetN

transWidgetM
    :: (fixed1 -> fixed2)
    -> (transition1 -> transition2)
    -> WidgetM fixed1 transition1 s t
    -> WidgetM fixed2 transition2 s t
transWidgetM transF transT (WidgetM cw) =
    WidgetM ((fmap . fmap) (bimap transF (transTransition transT)) cw)

transWidgetN
    :: (fixed1 -> fixed2)
    -> (transition1 -> transition2)
    -> WidgetN fixed1 transition1 t
    -> WidgetN fixed2 transition2 t
transWidgetN transF transT (WidgetN ui) =
    WidgetN ((fmap . fmap) (bimap transF (transTransition transT)) ui)

transWidgetMN
    :: forall fixed transition .
       ( )
    => (forall s t . WidgetM fixed transition s t -> Kleisli (FlowContinuation (WidgetN fixed transition)) s t)
transWidgetMN (widgetM  :: WidgetM fixed transition s t) = Kleisli $ \s -> 
    let cw :: ClosedWidget s (Event (Union fixed (Transition transition t)))
        cw = runWidgetM widgetM
        ui :: UI (Event (Union fixed (Transition transition t)))
        ui = lmap (const s) cw
    in  FlowNext (WidgetN ((fmap . fmap . fmap . fmap) FlowDone ui))

runWidgetFlow'
    :: forall fixed transition t .
       ( )
    => (UI (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))) -> UI (Event (Union fixed (Transition transition t))))
    -> FlowContinuation (WidgetN fixed transition) t
    -> Either t (UI (Event (Union fixed (Transition transition t))))
runWidgetFlow' _ (FlowDone t) = Left t
runWidgetFlow' contractor (FlowNext next) = 
    let ui :: UI (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t))))
        ui = runWidgetN next
    in  Right (contractor ui)

-- This one, along with widgetFlow below it, exhibit some useful symmetry.
-- Run a flow down to the FlowKleisli level, pass it through runWidgetFlow,
-- then through widgetFlow, and then throw on any arrow transformers as
-- necessary. It's like openWidget/closeWidget.
runWidgetFlow
    :: forall fixed transition s t .
       ( )
    => (UI (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))) -> UI (Event (Union fixed (Transition transition t))))
    -> Kleisli (FlowContinuation (WidgetN fixed transition)) s t
    -> (s -> Either t (UI (Event (Union fixed (Transition transition t)))))
runWidgetFlow contractor kleisli = \s ->
    runWidgetFlow' contractor (runKleisli kleisli s)

widgetFlow
    :: forall fixed transition s t .
       ( )
    => (s -> Either t (UI (Event (Union fixed (Transition transition t)))))
    -> Flow (WidgetM fixed transition) s t
widgetFlow mk = proc s -> do
    case mk s of
        Left t -> do returnA -< t
        Right ui -> do app -< (FlowM (WidgetM ui), ())

-- | Interpret a Flow using a particular container.
--
--     1. The container must isolate one particular child. It need not always
--        have only one child, but it must always have *at least one* and it
--        must be possible to get a hold of it.
--     2. One UI must be enough to make a value of the container (it can have
--        exactly one child).
--     3. One f-parameterized UI must be enough to make a change to that
--        container. After the change, the focus of the container (item 1)
--        must be that UI.
--
--   FIXME this is very daunting, but it's just general enough to unify the
--   simple and directed flows. Can we make it simpler and easier to understand?
runGeneralWidgetFlow
    :: forall tag container fixed transition t .
       ( ChildrenContainer (container (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))) () (Event (Union fixed (Transition transition t))))
       )
    => (container (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))) () (Event (Union fixed (Transition transition t))) Child
       -> Child (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))))
    -> (UI (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t))))
       -> container (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))) () (Event (Union fixed (Transition transition t))) SetChild)
    -> (Transition transition (UI (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))))
       -> [Change (container (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))) () (Event (Union fixed (Transition transition t)))) SetChild])
    -> (forall t . Event (Transition transition t) -> ElementBuilder tag ())
    -- ^ You can use the event of *all* transitions to alter the widget, but
    --   that event doesn't come out; only the final transition is given.
    -> UI (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t))))
    -> Widget tag () (Event (Union fixed (Transition transition t)))
runGeneralWidgetFlow getChild setChildI setChildC builder ui = widget $

    \(_, viewChildren :: ViewChildren (container (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))) () (Event (Union fixed (Transition transition t))))) -> do

    let first :: Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))
        first = childData . getChild $ viewChildrenInitial viewChildren
    let rest :: Event (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t))))
        rest = childData . getChild <$> viewChildrenEvent viewChildren
    continuations :: Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))
        <- sequenceSwitchE (first |> rest)

    let lefts :: Event fixed
        rights :: Event (Transition transition (FlowContinuation (WidgetN fixed transition) t))
        (lefts, rights) = esplit continuations

    let nexts :: Event (Either (Transition transition t) (Transition transition (UI (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))))))
        nexts = fmap (splitTransition . fmap takeNext) rights

    let final :: Event (Transition transition t)
        transitions :: Event (Transition transition (UI (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t))))))
        (final, transitions) = split nexts

    let output :: Event (Union fixed (Transition transition t))
        output = eunion lefts final

    let kids :: Children (container (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))) () (Event (Union fixed (Transition transition t))))
        kids = children (setChildI ui)
                        (setChildC <$> transitions)

    _ <- builder rights

    pure $ (output, kids)

-- |
takeNext
    :: forall fixed transition t .
       ( )
    => FlowContinuation (WidgetN fixed transition) t
    -> Either t (UI (Event (Union fixed (Transition transition (FlowContinuation (WidgetN fixed transition) t)))))
takeNext term = case term of
    FlowDone t -> Left t
    FlowNext next -> Right (runWidgetN next)

-- | A simple flow interpretation. The current flow piece is the sole child.
--   Old flow pieces are discarded.
runSimpleWidgetFlow
    :: forall tag fixed t .
       ( )
    => UI (Event (Union fixed (SimpleTransition (FlowContinuation (WidgetN fixed ()) t))))
    -> Widget tag () (Event (Union fixed (SimpleTransition t)))
runSimpleWidgetFlow = runGeneralWidgetFlow getChild setChildI setChildC (const (pure ()))

  where

    getChild :: forall r s t f . Single r s t f -> f r
    getChild = runSingle

    setChildI :: forall r s t . UI r -> Single r s t SetChild
    setChildI = Single . newChild 

    setChildC :: forall r s t . SimpleTransition (UI r) -> [Change (Single r s t) SetChild]
    setChildC = pure . Single . newChild . runTransition
