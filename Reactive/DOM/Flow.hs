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

module Reactive.DOM.Flow (

      Flow(Flow)
    , pureFlow
    , impureFlow
    , varyingFlow
    , widgetFlow
    , widgetFlow1
    , flowMap
    , alterFlow
    , alterFlow'
    , CompleteFlow
    , runFlow

    ) where

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
import Reactive.DOM.Node
import Reactive.DOM.Widget

-- | Description of a user interface flow: taking an s, producing a t, with
--   a side-channel producing an o.
data Flow o s t where
    -- | Do some MomentIO computation and either pass a value immediately, or
    --   show a Widget and pass the value through an Event.
    --   This constructor gives a *lot* of leeway: you can do any IO you
    --   want. But be careful! A useful Flow should never appear to be blocked.
    --   So don't put, for example, a synchronous HTTP request in here. Instead,
    --   throw in a Widget with an Event which fires when the request is done.
    Flow :: (s -> Either (MomentIO t) (WidgetConstructor (o, Event t))) -> Flow o s t
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
    id = Flow $ Left . pure
    (.) = FlowCompose

instance Arrow (Flow o) where
    arr = pureFlow
    first = FlowFirst

instance ArrowChoice (Flow o) where
    left = FlowLeft

instance ArrowApply (Flow o) where
    app = FlowApp

-- TODO get rid of widgetFlow1 and use this only.
-- Also change to use this signature:
-- widgetFlow :: (s -> WidgetConstructor (o, Event t)) -> Flow o s t
widgetFlow = widgetFlow1

-- | Make a Flow which shows one Widget.
widgetFlow1 :: (s -> MomentIO (Widget o, Event t)) -> Flow o s t
widgetFlow1 mk = Flow $ Right . mk'
  where
    mk' s = do
        (w, e) <- mk s
        let w' = (\o -> (o, e)) <$> w
        pure $ w'

-- | Make a flow which shows no Widget, just runs a pure function.
--   pureFlow = arr
pureFlow :: (s -> t) -> Flow o s t
pureFlow f = Flow $ Left . pure . f

-- | Make a flow which shows no Widget, just runs an impure function.
--   Careful: you should not do anything blocking here, or the Flow will be
--   interrupted (will not flow continuously from Widget to Widget).
impureFlow :: (s -> MomentIO t) -> Flow o s t
impureFlow f = Flow $ Left . f

varyingFlow :: Sequence (Flow o s t) -> MomentIO (Flow o s t)
varyingFlow seqnc = do
    (first, rest) <- runSequence seqnc
    b <- stepper first rest
    pure $ FlowVarying (b, rest)

flowMap :: (o -> o') -> Flow o s t -> Flow o' s t
flowMap f flow = case flow of
    FlowCompose l r -> FlowCompose (flowMap f l) (flowMap f r)
    FlowFirst fst -> FlowFirst (flowMap f fst)
    FlowLeft left -> FlowLeft (flowMap f left)
    Flow mk -> Flow $ \s -> case mk s of
        Left mkPure -> Left mkPure
        Right mkW -> Right $ (\w -> (\(o, ev) -> (f o, ev)) <$> w) <$> mkW

{-
splitArrow
    :: ( ArrowChoice arr )
    => arr s (Either t u)
    -> arr t s
    -> arr u r
    -> arr s r
splitArrow fl ifLeft ifRight = proc s -> do
    out <- fl -< s
    case out of
        Left t  -> do s <- ifLeft -< t
                      splitArrow fl ifLeft ifRight -< s
        Right u -> do r <- ifRight -< u
                      returnA -< r
-}

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

{-
newtype NextComponent o = NextComponent {
      runNextComponent :: Sequence (Widget o, Event (MomentIO (NextComponent o)))
    }

runNextComponents
    :: forall o .
       NextComponent o
    -> MomentIO (Sequence (Widget o))
runNextComponents nextComponent = do

    let seqnc :: Sequence (Widget o, Event (MomentIO (NextComponent o)))
        seqnc = runNextComponent nextComponent

    let seqncWidget :: Sequence (Widget o)
        seqncWidget = fst <$> seqnc

    let seqncEvent :: Sequence (Event (MomentIO (NextComponent o)))
        seqncEvent = snd <$> seqnc

    switchedSeqncEvent :: Event (MomentIO (NextComponent o))
        <- sequenceSwitchE seqncEvent

    executedSeqncEvent :: Event (NextComponent o)
        <- execute switchedSeqncEvent

    nexts :: Event (Sequence (Widget o))
        <- execute (runNextComponents <$> executedSeqncEvent)

    let whole :: Sequence (Sequence (Widget o))
        whole = seqncWidget |> nexts

    pure (sequenceSwitch whole)

    {-
    let nexts :: Event (Sequence (Sequence (Widget o)))
        nexts = . fmap runNextComponents <$> event

    remainingWidgets :: Event (Widget o)

    initialWidget |> remainingWidgets
    -}

    {-
    commutedEventSequence :: Sequence (Event (Sequence (NextComponent o)))
        <- sequenceCommute eventSequence

    commutedEvent :: Event (Sequence (NextComponent o))
        <- sequenceSwitchE commutedEventSequence

    let nexts :: Event (MomentIO (Sequence (Widget o)))
        nexts = runNextComponents <$> commutedEvent

    commutedNexts :: Event (Sequence (Widget o))
        <- execute nexts

    let switched = sequenceSwitch (initialWidgets |> commutedNexts)

    sequenceReactimate (const (print "seqnc changing") <$> seqnc)
    sequenceReactimate (const (print "initialWidgets changing") <$> initialWidgets)
    sequenceReactimate (const (print "eventSequence changing") <$> eventSequence)
    sequenceReactimate (const (print "commutedEventSequence changing") <$> commutedEventSequence)
    reactimate (const (print "commutedEvent changing") <$> commutedEvent)
    reactimate (const (print "commutedNexts changing") <$> commutedNexts)
    sequenceReactimate (const (print "switched changing") <$> switched)

    pure switched
    -}

-- | If we can come up with the next component from t, then we can come up
--   with the next component from s: run the flow until a Flow is found, at
--   which point we have a Widget and an Event.
--
--   Note that if t ~ Void then we can use absurd for the k parameter.
nextComponentK
    :: forall o s t .
       Flow o s t
    -> (t -> MomentIO (NextComponent o))
    -> (s -> MomentIO (NextComponent o))
nextComponentK flow k = case flow of
    Flow mk -> \s -> case mk s of
        Left mkT -> do
            t <- mkT
            k t
        Right mkW -> do
            ((o, ev), velem) <- runWidget <$> mkW
            pure $ NextComponent (always (Widget (o, velem), k <$> ev))
    FlowVarying (b, ev) -> \s -> do
        -- Whenever ev fires, we get a new flow.
        -- The resulting sequence is controlled by this event. Whenever it
        -- fires, the sequence switches to a new sequence determined by that
        -- flow.
        -- But this is wrong. We need to come up with one single
        -- NextComponent (type signature of nextComponentK should change to
        -- use  MomentIO (NextComponent o)
        -- That NextComponent contains a sequence, which should be the
        -- switched sequence, but how to get a (Widget o, Event t)? We can
        -- come up with a NextComponent o for each element in the sequence.
        -- Then we must union these? Aha, extract their sequence, switch, put
        -- that into the output sequence!
        {-
        current :: Flow o s t <- valueB b
        let seqnc :: Sequence (Flow o s t)
            seqnc = rstepper current ev
        let nexts :: Sequence (MomentIO (Sequence (NextComponent o)))
            nexts = (\flow -> nextComponentK flow k s) <$> seqnc
        commuted :: Sequence (Sequence (NextComponent o))
            <- sequenceCommute nexts
        let switched = sequenceSwitch commuted
        reactimate (const (print "Varying flow event changing") <$> ev)
        sequenceReactimate (const (print "Varying flow commuted changing") <$> commuted)
        sequenceReactimate (const (print "Varying flow switched changing") <$> switched)
        pure switched
        -}
        -- Following is *still* wrong.
        -- We must some determine when each flow is *really* finished; not when
        -- its internal widgets change, but when the whole thing is terminated,
        -- for only then do we move control completely to the next flow.
        --
        current :: Flow o s t <- valueB b
        let seqnc :: Sequence (Flow o s t)
            seqnc = rstepper current ev
        let nexts :: Sequence (MomentIO (NextComponent o))
            nexts = (\flow -> nextComponentK flow k s) <$> seqnc
        commuted :: Sequence (NextComponent o)
            <- sequenceCommute nexts
        let unswitched :: Sequence (Sequence (Widget o, Event (MomentIO (NextComponent o))))
            unswitched = runNextComponent <$> commuted
        let switched :: Sequence (Widget o, Event (MomentIO (NextComponent o)))
            switched = sequenceSwitch unswitched
        pure $ NextComponent switched
    FlowCompose left right ->
        let k' = nextComponentK left k
        in  nextComponentK right k'
    FlowFirst subFlow -> \(s, c) -> do
        nextComponentK subFlow (\t -> k (t, c)) s
    FlowLeft subFlow -> \sum -> do
        case sum of
            Left x -> nextComponentK subFlow (\t -> k (Left t)) x
            Right y -> k (Right y)
    FlowApp -> \(flowST, s) -> do
        nextComponentK flowST k s

flowSequence
    :: Flow o s t
    -> (t -> MomentIO (NextComponent o))
    -> s
    -> MomentIO (Sequence (Widget o))
flowSequence flow k s = do
    nextComponent <- nextComponentK flow k s 
    runNextComponents nextComponent

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
       Flow o s t
    -> s
    -> MomentIO (Either t (Sequence (Widget o), Event t))
runFlowGeneral flow = case flow of
    FlowApp -> \(flow', s) -> runFlowGeneral flow' s
    FlowCompose (left :: Flow o u t) (right :: Flow o s u) -> \s -> do
        right' <- runFlowGeneral right s
        case right' of
            Left t -> runFlowGeneral left t
            Right (seqnc, ev) -> do
                let next :: Event (MomentIO (Either t (Sequence (Widget o), Event t)))
                    next = runFlowGeneral left <$> ev
                executed :: Event (Either t (Sequence (Widget o), Event t))
                    <- execute next
                let split :: (Event (Sequence (Widget o)), Event t)
                    split = splitEvent executed
                let otherSeqncs = fst split
                let finalSeqnc = sequenceSwitch (seqnc |> otherSeqncs)
                pure $ Right (finalSeqnc, snd split)
    FlowFirst subFlow -> \(s, c) -> do
        out <- runFlowGeneral subFlow s
        pure $ case out of
            Left t -> Left (t, c)
            Right (seqnc, ev) -> Right (seqnc, (\t -> (t, c)) <$> ev)
    FlowLeft subFlow -> \choice -> case choice of
        Right c -> pure $ Left (Right c)
        Left s -> do
            out <- runFlowGeneral subFlow s
            pure $ case out of
                Left t -> Left (Left t)
                Right (seqnc, ev) -> Right (seqnc, (\t -> Left t) <$> ev)
    FlowVarying (be, ev) -> \s -> do
        current :: Flow o s t <- valueB be
        let seqnc :: Sequence (Flow o s t)
            seqnc = rstepper current ev
        let nexts :: Sequence (MomentIO (Either t (Sequence (Widget o), Event t)))
            nexts = (\flow -> runFlowGeneral flow s) <$> seqnc
        commuted :: Sequence (Either t (Sequence (Widget o), Event t))
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
                let widgetSequence :: Sequence (Sequence (Widget o))
                    widgetSequence = seqnc |> (fst <$> filterJust (either (const Nothing) Just <$> rest))
                pure $ Right (sequenceSwitch widgetSequence, nextEvent)
    Flow mk -> \s -> case mk s of
        Left mkT -> Left <$> mkT
        Right mkW -> do
            ((o, ev), velem) <- runWidget <$> mkW
            pure $ Right (always (Widget (o, velem)), ev)


-- | Produce a virtual element which shows a complete flow, along with the
--   switched output.
runFlow
    :: Flow o s Void
    -> s 
    -> WidgetConstructor (Sequence o)
runFlow flow s = do
    -- This pattern is exhaustive; Left Void cannot be constructed.
    Right (widgets, mustBeNever) <- runFlowGeneral flow s
    let seqnc = runWidget <$> widgets
    let outs = fst <$> seqnc
    let velems = snd <$> seqnc
    velem <- virtualElement ("div")
                            (always mempty)
                            (always mempty)
                            (always mempty)
                            (pure . node <$> velems)
    pure $ Widget (outs, velem)
