{-|
Module      : Reactive.DOM.Flow
Description : Definition of Flows.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Reactive.DOM.Internal.Flow where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Category
import Control.Arrow
import Control.Monad.Trans.Class (lift)
import Data.Void
import Data.Profunctor
import Data.Functor.Compose
import Data.Monoid (mempty)
import Data.Algebraic.Index
import Data.Algebraic.Sum
import Data.Algebraic.Product hiding (Component)
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Internal.Tag (W3CTag)
import Reactive.DOM.Internal.Node
import Reactive.DOM.Children.Single

-- | Description of a user interface flow: taking an s, producing a t, with
--   a side-channel producing an o.
data Flow o s t where
    FlowMoment :: (s -> MomentIO t) -> Flow o s t
    FlowWidget :: W3CTag tag => Widget tag s (o, Event t) -> Flow o s t
    -- | Symbolic composition.
    FlowCompose :: Flow o u t -> Flow o s u -> Flow o s t
    -- | Symbol first, for Arrow.
    FlowFirst :: Flow o s t -> Flow o (s, c) (t, c)
    -- | Symbol left, for ArrowChoice.
    FlowLeft :: Flow o s t -> Flow o (Either s c) (Either t c)
    FlowApp :: Flow o (Flow o s t, s) t

-- | A @CompleteFlow@ is a neverending @Flow@. Give an input @s@ and you'll
--   have a non-stop user interface. Only this kind of @Flow@ can be run to
--   produce a @VirtualElement@ (see @runFlow@).
type CompleteFlow o s = Flow o s Void

instance Category (Flow o) where
    id = pureFlow id
    (.) = FlowCompose

instance Arrow (Flow o) where
    arr = pureFlow
    first = FlowFirst

instance ArrowChoice (Flow o) where
    left = FlowLeft

instance ArrowApply (Flow o) where
    app = FlowApp

-- | Make a flow which shows no Widget, just runs a pure function.
--   pureFlow = arr
pureFlow :: (s -> t) -> Flow o s t
pureFlow f = FlowMoment $ pure . f

-- | Make a flow which shows no Widget, just runs an impure function.
--   Careful: you should not do anything blocking here, or the Flow will be
--   interrupted (will not flow continuously from Widget to Widget).
impureFlow :: (s -> MomentIO t) -> Flow o s t
impureFlow = FlowMoment

widgetFlow :: W3CTag tag => (Widget tag s (o, Event t)) -> Flow o s t
widgetFlow = FlowWidget

widgetFlow' :: W3CTag tag => Widget tag s (Event t) -> Flow o (s, o) t
widgetFlow' w = FlowWidget $ rmap (\(ev, o) -> (o, ev)) (passthrough w)

flowMap :: (o -> o') -> Flow o s t -> Flow o' s t
flowMap f flow = case flow of
    FlowCompose l r -> FlowCompose (flowMap f l) (flowMap f r)
    FlowFirst fst -> FlowFirst (flowMap f fst)
    FlowLeft left -> FlowLeft (flowMap f left)
    FlowMoment m -> FlowMoment m
    FlowWidget mk -> FlowWidget (fmap (\(o, ev) -> (f o, ev)) mk)
    FlowApp -> proc (flow, s) -> do
        FlowApp -< (flowMap f flow, s)

{-

-- TODO this one is really a mess and hard to follow.
alterFlow
    :: forall o s t u final.
       -- Every WidgetConstructor in the flow is modified, probably adding new
       -- Widgets containing Events.
       (  forall t .
          WidgetConstructor (o, Event t)
       -> WidgetConstructor (o, Event (Either u t))
       )
    -- To handle the Left events created by the first parameter, 
    -- Think carefully about this one. We want to be able to express
    --   - continue where we left off (with possible pre-processing)
    --   - go back to the start
    --   - fuck it, finish the *entire* flow (of which the input flow is only
    --     a piece).
    -> (forall s t . Flow o s t -> Flow o (u, s) (Either final t))
    -> Flow o s t
    -> Flow o (Either final s) (Either final t)
alterFlow fwidget fflow flow = case flow of

    -- TBD is this correct?
    FlowApp -> proc choice -> do
        case choice of
            Left final -> do
                returnA -< Left final
            Right (flow', s) -> do
                let altered = alterFlow fwidget fflow flow'
                app -< (altered, Right s)
    -- Surely the following would be wrong.
    --FlowApp -> id +++ FlowApp

    FlowCompose left right ->
        let left' = alterFlow fwidget fflow left
            right' = alterFlow fwidget fflow right
            dis = id ||| left'
        in  FlowCompose left' right'

    FlowFirst subFlow ->
        let subFlow' = alterFlow fwidget fflow subFlow
        in  proc inp -> do
                case inp of
                    Left final -> do
                        returnA -< Left final
                    Right (s, c) -> do
                        out <- subFlow' -< Right s
                        returnA -< (\t -> (t, c)) <$> out

    FlowLeft subFlow ->
        let subFlow' = alterFlow fwidget fflow subFlow
        in  proc inp -> do
                case inp of
                    Left final -> do
                        returnA -< Left final
                    Right choice -> case choice of
                        Left s -> do
                            out <- subFlow' -< Right s
                            returnA -< Left <$> out
                        Right c -> do
                            returnA -< Right (Right c)

    {-
    FlowVarying (be, ev) ->
        let be' = alterFlow fwidget fflow <$> be
            ev' = alterFlow fwidget fflow <$> ev
        in  FlowVarying (be', ev')
    -}

    Flow mk ->
        let altered = Flow $ \s -> case mk s of
                Left mkT -> Left $ Right <$> mkT
                Right mkW -> Right $ fwidget mkW
            escape = fflow (Flow mk)
        in  proc choice -> do
                case choice of
                    Left final -> do
                        returnA -< Left final
                    Right s -> do
                        out <- altered -< s
                        case out of
                            Left u -> do
                                escape -< (u, s)
                            Right t -> do
                                returnA -< Right t

alterFlow'
    :: forall o s u final.
       (  forall t .
          WidgetConstructor (o, Event t)
       -> WidgetConstructor (o, Event (Either u t))
       )
    -> (forall s t . Flow o s t -> Flow o (u, s) (Either final t))
    -> Flow o s final
    -> Flow o s final
alterFlow' fwidget fflow flow =
    let altered = alterFlow fwidget fflow flow
    in  proc s -> do
            out <- altered -< Right s
            returnA -< either id id out

-- | Like alterFlow but whenever a Left event comes, one particular flow
--   is run, with no chance to use the continuation Flow.
alterFlowUniform
    :: forall o s t u final.
       (  forall t .
          WidgetConstructor (o, Event t)
       -> WidgetConstructor (o, Event (Either u t))
       )
    -> Flow o u t
    -> Flow o s t
    -> Flow o s t
alterFlowUniform fwidget fflow flow =
    arr Right >>> alterFlow fwidget fflow' flow >>> arr (either id id)
  where
    fflow' :: forall s t' . Flow o s t' -> Flow o (u, s) (Either t t')
    fflow' _ = arr fst >>> fflow >>> arr Left
-}

newtype FlowContinuation o r = FlowContinuation {
      runFlowContinuation :: UI (o, Event (MomentIO (Either r (FlowContinuation o r))))
    }

runFlowGeneral
    :: forall o s t r .
       Flow o s t
    -> (t -> MomentIO (Either r (FlowContinuation o r)))
    -> s
    -> MomentIO (Either r (FlowContinuation o r))
runFlowGeneral flow k = case flow of

    FlowMoment f -> \s -> do
        t <- f s
        k t

    FlowWidget mk -> \s ->
        let theUI = ui (lmap (const s) mk)
        in  pure (Right (FlowContinuation ((fmap . fmap . fmap) k theUI)))

    FlowApp -> \(flow', s) -> runFlowGeneral flow' k s

    FlowFirst subFlow -> \(s, c) -> do
        runFlowGeneral subFlow (\t -> k (t, c)) s

    FlowLeft subFlow -> \choice -> case choice of
        Right c -> k (Right c)
        Left s -> runFlowGeneral subFlow (k . Left) s

    FlowCompose (left :: Flow o u t) (right :: Flow o s u) -> \s -> do
        let k' = runFlowGeneral left k
        runFlowGeneral right k' s

-- TBD Maybe runFlow should just give an Intrinsic part, rather than choosing
-- the "div" tag.
--
-- If we give a Sequence Extrinsic o then it's useless. Must transform it to
-- MomentIO.
-- Perhaps put an m parameter on the Widget and use this to fix the Sequence's
-- m parameter?
runFlow
    :: forall o s .
       Flow o s Void
    -> OpenWidget s (Sequence MomentIO o)
runFlow flow = widget $ \(s, viewChildren) -> do

    let first :: (o, Event (MomentIO (Either Void (FlowContinuation o Void))))
        rest :: Event (o, Event (MomentIO (Either Void (FlowContinuation o Void))))
        first = childData . runSingle $ viewChildrenInitial viewChildren
        rest = childData . runSingle <$> (viewChildrenEvent viewChildren)

    -- A sequence of events, each of which gives a MomentIO producing
    -- the next Widget to show and the next continuation.
    let conts :: Sequence MomentIO (Event (MomentIO (FlowContinuation o Void)))
        conts =  ((fmap . fmap) (either absurd id) . snd  $ first)
              |> ((fmap . fmap) (either absurd id) . snd <$> rest)
    changeEvents :: Event (FlowContinuation o Void)
        <- liftMomentIO (sequenceSwitchE conts >>= execute)

    -- We can take the first FlowContinuation. We know it's there because
    -- the Left variant from runFlowGeneral is Void.
    x <- liftMomentIO (runFlowGeneral flow absurd s)
    let fk :: FlowContinuation o Void
        fk = either absurd id x

    -- The first child.
    let uiFirst :: UI (o, Event (MomentIO (Either Void (FlowContinuation o Void))))
        uiFirst = runFlowContinuation fk
    -- The remaining children, defined via changeEvents and ultimately
    -- derived from the input (first, rest).
    let uiRest :: Event (UI (o, Event (MomentIO (Either Void (FlowContinuation o Void)))))
        uiRest = runFlowContinuation <$> changeEvents

    -- Using the data from the rendered children, we have the output data
    -- readily available: just strip off theFlowContinuation parts.
    let values = (fst first) |> (fst <$> rest)

    let kids = children (Single (newChild uiFirst))
                        (pure . Single . newChild <$> uiRest)

    -- State the output values and children.
    pure $ (values, kids)
