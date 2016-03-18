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
import Data.Bifunctor (bimap)
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
--   a side-channel producing an o. A note on the side channel: it's a way to
--   observe data from the widgets within the flow.
data Flow o s t where
    FlowMoment :: (s -> MomentIO t) -> Flow o s t
    FlowWidget :: W3CTag tag => Widget tag s (Maybe o, Event t) -> Flow o s t
    -- | Symbolic composition.
    FlowCompose :: Flow o u t -> Flow o s u -> Flow o s t
    -- | Symbol first, for Arrow.
    FlowFirst :: Flow o s t -> Flow o (s, c) (t, c)
    -- | Symbol left, for ArrowChoice.
    FlowLeft :: Flow o s t -> Flow o (Either s c) (Either t c)
    FlowApp :: Flow o (Flow o s t, s) t
    -- | Open up a closed flow by promoting a side-channel event to the
    --   control event.
    FlowOpen :: Flow (Event t) s Void -> Flow o s t

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

widgetFlow :: W3CTag tag => (Widget tag s (Maybe o, Event t)) -> Flow o s t
widgetFlow = FlowWidget

widgetFlow' :: W3CTag tag => Widget tag s (Event t) -> Flow o s t
widgetFlow' w = FlowWidget $ rmap (\ev -> (Nothing, ev)) w

-- | Open a complete (closed) flow by using its side-channel event as its
--   control event. 
openFlow :: Flow (Event t) s Void -> Flow anything s t
openFlow = FlowOpen

flowMap :: (o -> o') -> Flow o s t -> Flow o' s t
flowMap f = flowTrans (pure . fmap f)

-- | For flows which terminate in a Widget, alter the event which determines
--   when that flow ends. Your alteration is in Moment, so you can work with
--   steppers.
flowMapE :: forall o s t . (Event t -> Moment (Event t)) -> Flow o s t -> Flow o s t
flowMapE f flow = case flow of
    FlowCompose l r -> FlowCompose (flowMapE f l) r
    FlowFirst first -> FlowFirst first
    FlowLeft left -> FlowLeft left
    FlowMoment m -> FlowMoment m
    FlowWidget w -> FlowWidget $
        w `modifyr` (modifier $ \_ (o, ev) -> liftMoment (f ev) >>= pure . (,) o)
    FlowApp -> proc (flow, s) -> do
        FlowApp -< (flowMapE f flow, s)
    FlowOpen closed -> FlowOpen (flowTrans (traverse f) closed)

flowTrans :: forall o o' s t . (Maybe o -> Moment (Maybe o')) -> Flow o s t -> Flow o' s t
flowTrans f flow = case flow of
    FlowCompose l r -> FlowCompose (flowTrans f l) (flowTrans f r)
    FlowFirst first -> FlowFirst (flowTrans f first)
    FlowLeft left -> FlowLeft (flowTrans f left)
    FlowMoment m -> FlowMoment m
    FlowWidget w -> FlowWidget $
        w `modifyr` (modifier $ \_ (o, ev) -> liftMoment (f o) >>= pure . flip (,) ev)
    FlowApp -> proc (flow, s) -> do
        FlowApp -< (flowTrans f flow, s)
    FlowOpen closed -> FlowOpen closed

-- | A tool to implement runFlowGeneral, ultimately producing a sequence of
--   UIs, and thereby an OpenWidget, from a flow.
newtype FlowContinuation o r = FlowContinuation {
      runFlowContinuation :: UI (Maybe o, Event (MomentIO (Either r (FlowContinuation o r))))
    }

-- | Run an arbitrary flow through a continuation.
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

    FlowOpen closed -> \s -> do
        -- closed is a complete flow, so we're free to choose whatever we want
        -- for the final output type (r as it appears in the type signature
        -- for runFlowGeneral). Turns out it's rather convenient to choose Void.
        let k' :: Void -> MomentIO (Either Void (FlowContinuation (Event t) Void))
            k' = absurd
        flowCont <- runFlowGeneral closed k' s
        case flowCont of
            Left r -> absurd r
            Right (FlowContinuation r) -> mdo
                -- We want to keep on with the flow continuation recovered here,
                -- but squelch its side-channel part (the first component) by
                -- setting it to Nothing, and switch it through k as soon as
                -- one of those first component side-channel events fires.
                let f :: (Maybe (Event t), Event (MomentIO (Either Void (FlowContinuation (Event t) Void))))
                      -> (Maybe o, Event (MomentIO (Either r (FlowContinuation o r))))
                    f (maybeEv, rest) =
                        let initial :: Event t
                            initial = maybe never id maybeEv
                            appliedInitial :: Event (MomentIO (Either r (FlowContinuation o r)))
                            appliedInitial = fmap k initial
                            next :: Event (MomentIO (Either r (FlowContinuation o r)))
                            next = fmap k initial
                            recurse :: FlowContinuation (Event t) Void -> FlowContinuation o r
                            recurse = FlowContinuation . fmap f . runFlowContinuation
                            appliedRecurse :: Event (MomentIO (Either r (FlowContinuation o r)))
                            appliedRecurse = (fmap . fmap) (bimap absurd recurse) rest
                            ev = unionWith const appliedInitial appliedRecurse
                        in  (Nothing, ev)
                pure (Right (FlowContinuation (fmap f r)))

-- | Compile a flow to an OpenWidget.
runFlow
    :: forall o s .
       Flow o s Void
    -> OpenWidget s (Sequence (Maybe o))
runFlow flow = widget $ \(s, viewChildren) -> do

    let first :: (Maybe o, Event (MomentIO (Either Void (FlowContinuation o Void))))
        rest :: Event (Maybe o, Event (MomentIO (Either Void (FlowContinuation o Void))))
        first = childData . runSingle $ viewChildrenInitial viewChildren
        rest = childData . runSingle <$> (viewChildrenEvent viewChildren)

    -- A sequence of events, each of which gives a MomentIO producing
    -- the next Widget to show and the next continuation.
    let conts :: Sequence (Event (MomentIO (FlowContinuation o Void)))
        conts =  ((fmap . fmap) (either absurd id) . snd  $ first)
              |> ((fmap . fmap) (either absurd id) . snd <$> rest)
    switchedConts :: Event (MomentIO (FlowContinuation o Void))
        <- liftMoment $ sequenceSwitchE conts
    changeEvents :: Event (FlowContinuation o Void)
        <- liftMomentIO $ execute switchedConts

    -- We can take the first FlowContinuation. We know it's there because
    -- the Left variant from runFlowGeneral is Void.
    x <- liftMomentIO (runFlowGeneral flow absurd s)
    let fk :: FlowContinuation o Void
        fk = either absurd id x

    -- The first child.
    let uiFirst :: UI (Maybe o, Event (MomentIO (Either Void (FlowContinuation o Void))))
        uiFirst = runFlowContinuation fk
    -- The remaining children, defined via changeEvents and ultimately
    -- derived from the input (first, rest).
    let uiRest :: Event (UI (Maybe o, Event (MomentIO (Either Void (FlowContinuation o Void)))))
        uiRest = runFlowContinuation <$> changeEvents

    -- Using the data from the rendered children, we have the output data
    -- readily available: just strip off theFlowContinuation parts.
    let values = (fst first) |> (fst <$> rest)

    let kids = children (Single (newChild uiFirst))
                        (pure . Single . newChild <$> uiRest)

    -- State the output values and children.
    pure $ (values, kids)
