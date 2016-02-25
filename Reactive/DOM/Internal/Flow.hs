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
import Data.Void
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Monoid (mempty)
import Data.Algebraic.Index
import Data.Algebraic.Sum
import Data.Algebraic.Product hiding (Component)
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Internal.Node

-- | Description of a user interface flow: taking an s, producing a t, with
--   a side-channel producing an o.
data Flow o s t where
    FlowMoment :: (s -> MomentIO t) -> Flow o s t
    FlowWidget :: Foldable f => (s -> Widget f (o, Event t)) -> Flow o s t
    -- | A time-varying Flow. The Behavior must be derived from the Event
    --   via stepper.
    FlowVarying :: (Behavior (Flow o s t), Event (Flow o s t)) -> Flow o s t
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

varyingFlow :: Sequence (Flow o s t) -> MomentIO (Flow o s t)
varyingFlow seqnc = do
    (first, rest) <- runSequence seqnc
    b <- stepper first rest
    pure $ FlowVarying (b, rest)

widgetFlow :: Foldable f => (s -> Widget f (o, Event t)) -> Flow o s t
widgetFlow = FlowWidget

widgetFlow' :: Foldable f => Widget f (Event t) -> Flow o o t
widgetFlow' mk = FlowWidget $ \o -> mk >>>= \ev -> ixpure (o, ev)

flowMap :: (o -> o') -> Flow o s t -> Flow o' s t
flowMap f flow = case flow of
    FlowCompose l r -> FlowCompose (flowMap f l) (flowMap f r)
    FlowFirst fst -> FlowFirst (flowMap f fst)
    FlowLeft left -> FlowLeft (flowMap f left)
    FlowVarying (be, ev) -> FlowVarying (flowMap f <$> be, flowMap f <$> ev)
    FlowMoment m -> FlowMoment m
    FlowWidget mk -> FlowWidget ((fmap . fmap) (\(o, ev) -> (f o, ev)) mk)
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

    FlowVarying (be, ev) ->
        let be' = alterFlow fwidget fflow <$> be
            ev' = alterFlow fwidget fflow <$> ev
        in  FlowVarying (be', ev')

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

splitEvent :: forall t r . Event (Either t (r, Event t)) -> (Event r, Event t)
splitEvent ev =
    let lefts :: Event t
        lefts = filterJust (either Just (const Nothing) <$> ev)
        rights :: Event t
        rights = switchE (filterJust (either (const Nothing) (Just . snd) <$> ev))
        rs :: Event r
        rs = filterJust (either (const Nothing) (Just . fst) <$> ev)
    in  (rs, unionWith const lefts rights)

-- | Since a Flow does not necessarily contain any Widgets, the general
--   run flow may give back a value (Left). Otherwise (Right), you get a
--   sequence of Widgets, and an Event which fires when the Flow is complete.
runFlowGeneral
    :: forall o s t .
       Document
    -> Flow o s t
    -> s
    -> MomentIO (Either t (Sequence (Element, o), Event t))
runFlowGeneral document flow = case flow of
    FlowApp -> \(flow', s) -> runFlowGeneral document flow' s
    FlowCompose (left :: Flow o u t) (right :: Flow o s u) -> \s -> do
        right' <- runFlowGeneral document right s
        case right' of
            Left t -> runFlowGeneral document left t
            Right (seqnc, ev) -> do
                let next :: Event (MomentIO (Either t (Sequence (Element, o), Event t)))
                    next = runFlowGeneral document left <$> ev
                executed :: Event (Either t (Sequence (Element, o), Event t))
                    <- execute next
                let split :: (Event (Sequence (Element, o)), Event t)
                    split = splitEvent executed
                let otherSeqncs = fst split
                let finalSeqnc = sequenceSwitch (seqnc |> otherSeqncs)
                pure $ Right (finalSeqnc, snd split)
    FlowFirst subFlow -> \(s, c) -> do
        out <- runFlowGeneral document subFlow s
        pure $ case out of
            Left t -> Left (t, c)
            Right (seqnc, ev) -> Right (seqnc, (\t -> (t, c)) <$> ev)
    FlowLeft subFlow -> \choice -> case choice of
        Right c -> pure $ Left (Right c)
        Left s -> do
            out <- runFlowGeneral document subFlow s
            pure $ case out of
                Left t -> Left (Left t)
                Right (seqnc, ev) -> Right (seqnc, (\t -> Left t) <$> ev)
    FlowVarying (be, ev) -> \s -> do
        current :: Flow o s t <- valueB be
        let seqnc :: Sequence (Flow o s t)
            seqnc = rstepper current ev
        let nexts :: Sequence (MomentIO (Either t (Sequence (Element, o), Event t)))
            nexts = (\flow -> runFlowGeneral document flow s) <$> seqnc
        commuted :: Sequence (Either t (Sequence (Element, o), Event t))
            <- sequenceCommute nexts
        (first, rest) <- runSequence commuted
        case first of
            -- First element has a value for us; use it and we're off.
            Left t -> pure $ Left t
            Right (seqnc, evNext) -> do
                -- The event to give, in case it's even necessary, shall be the event
                -- of whichever sequence of widgets is currently shown, unioned with
                -- an event which fires whenever a non-widget flow comes up.
                let nextEventRight :: Event t
                    nextEventRight = switchE (snd <$> filterJust (either (const Nothing) Just <$> rest))
                let nextEventLeft :: Event t
                    nextEventLeft = (filterJust (either Just (const Nothing) <$> rest))
                let nextEvent :: Event t
                    nextEvent = unionWith const nextEventRight nextEventLeft
                let widgetSequence :: Sequence (Sequence (Element, o))
                    widgetSequence = seqnc |> (fst <$> filterJust (either (const Nothing) Just <$> rest))
                pure $ Right (sequenceSwitch widgetSequence, nextEvent)
    FlowMoment f -> \s -> Left <$> f s
    FlowWidget mk -> \s -> do
        (el, (o, ev)) <- buildElement (mk s) document
        pure $ Right (always (el, o), ev)

-- | Produce a virtual element which shows a complete flow, along with the
--   switched output.
--   Notice the Identity parameter, indicating the Widget generated from
--   the flow will always have precisely one child.
runFlow
    :: forall o s .
       Flow o s Void
    -> s 
    -> Widget Identity (Sequence o)
runFlow flow s =
    getDocument >>>= \document ->
    momentIO (runFlowGeneral document flow s) >>>= \x ->
    let (seqnc, mustBeNever) = either absurd id x
        elems :: Sequence (Identity ElementSchemaChild)
        elems = Identity . Left . fst <$> seqnc
        outs :: Sequence o
        outs = snd <$> seqnc
    in  children (const elems) >>>= \_ ->
        ixpure outs
