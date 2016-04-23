{-|
Module      : Reactive.DOM.WidgetFlow
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

module Reactive.DOM.WidgetFlow where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Arrow.Flow
import Control.Monad (join)
import Data.Void
import Data.Profunctor
import Data.Bifunctor (Bifunctor, bimap)
import qualified Data.Bifunctor as Bifunctor (second)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Semigroup (Semigroup, Endo(..), (<>))
import Data.Monoid hiding ((<>))
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Internal.Mutation
import Reactive.DOM.Children.Single

import Data.Proxy

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

-- The transition functor is isomorphic to a Union where s is fixed.
data Union s t = ULeft s | URight t | UBoth s t
-- Ok so why not just fix to union? 
--
--   s -> UI (Event (Union fixed t))

instance Bifunctor Union where
    bimap f g term = case term of
        ULeft s -> ULeft (f s)
        URight t -> URight (g t)
        UBoth s t -> UBoth (f s) (g t)

instance Functor (Union s) where
    fmap = Bifunctor.second

instance Semigroup s => Applicative (Union s) where
    pure = URight
    mf <*> mx = do
        f <- mf
        x <- mx
        pure (f x)

instance Semigroup s => Monad (Union s) where
    return = pure
    mx >>= k = joinUnion (fmap k mx)

uGrowLeft :: (t -> Maybe u) -> Union s t -> Union (Union s u) t
uGrowLeft f term = case term of
    ULeft s -> ULeft (ULeft s)
    URight t -> case f t of
        Nothing -> URight t
        Just u -> UBoth (URight u) t
    UBoth s t -> case f t of
        Nothing -> UBoth (ULeft s) t
        Just u -> UBoth (UBoth s u) t

uGrowRight :: (s -> Maybe u) -> Union s t -> Union s (Union t u)
uGrowRight f term = uswap (uGrowLeft f (uswap term))

uswap :: Union s t -> Union t s
uswap term = case term of
    ULeft s -> URight s
    URight t -> ULeft t
    UBoth s t -> UBoth t s

elimUnion :: (s -> s -> s) -> Union s s -> s
elimUnion _ (ULeft s) = s
elimUnion _ (URight s) = s
elimUnion f (UBoth s1 s2) = f s1 s2

pickULeft :: Union s t -> Maybe s
pickULeft (ULeft s) = Just s
pickULeft (UBoth s _) = Just s
pickULeft (URight _) = Nothing

pickURight :: Union s t -> Maybe t
pickURight (URight t) = Just t
pickURight (UBoth _ t) = Just t
pickuRight (ULeft _) = Nothing

joinUnion :: Semigroup s => Union s (Union s t) -> Union s t
joinUnion (ULeft s) = ULeft s
joinUnion (URight (URight t)) = URight t
joinUnion (URight (ULeft s)) = ULeft s
joinUnion (URight (UBoth s t)) = UBoth s t
joinUnion (UBoth s (URight t)) = UBoth s t
joinUnion (UBoth s (ULeft s')) = ULeft (s <> s')
joinUnion (UBoth s (UBoth s' t)) = UBoth (s <> s') t

eunion :: forall s t . Event s -> Event t -> Event (Union s t)
eunion ss ts =
    let left :: Event (Union s t)
        left = ULeft <$> ss
        right :: Event (Union s t)
        right = URight <$> ts
        -- Partial match is safe (if it's not, reactive-banana is at fault).
        combine (ULeft s) (URight t) = UBoth s t
    in  unionWith combine left right

esplit :: Event (Union s t) -> (Event s, Event t)
esplit ev =
    let lefts = filterJust (pickULeft <$> ev)
        rights = filterJust (pickURight <$> ev)
    in  (lefts, rights)

eleft :: Event (Union s t) -> Event s
eleft = fst . esplit

eright :: Event (Union s t) -> Event t
eright = snd . esplit



newtype Transition d t = Transition {
      getTransition :: (d, t)
    }

instance Functor (Transition d) where
    fmap f = Transition . fmap f . getTransition

instance Monoid d => Applicative (Transition d) where
    pure t = Transition (mempty, t)
    Transition (d1, f) <*> Transition (d2, x) = Transition (d1 `mappend` d2, f x)

instance Monoid d => Monad (Transition d) where
    return = pure
    Transition (d1, x) >>= k =
        let Transition (d2, y) = k x
        in  Transition (d1 `mappend` d2, y)

type SimpleTransition = Transition ()

simpleTransition :: t -> SimpleTransition t
simpleTransition t = Transition ((), t)

transTransition :: (d1 -> d2) -> Transition d1 t -> Transition d2 t
transTransition trans (Transition (d1, t)) = Transition (trans d1, t)

splitTransition
    :: forall d s t .
       Transition d (Either s t)
    -> Either (Transition d s) (Transition d t)
splitTransition (Transition (d, choice)) = bimap mk mk choice
  where
    mk :: forall s . s -> Transition d s
    mk = Transition . (,) d

runTransition :: Transition d t -> t
runTransition = snd . getTransition

takeTransition :: Transition d t -> d
takeTransition = fst . getTransition

-- OK that's good: ditch the Transition class and just use the newtype.
-- Second item: returning other data from the widgets. We have a case in which
-- the widgets must give
--
--   1. Transitions
--   2. State endomorphisms
--   3. JumpTo Urls so we can reactimate a pushState
--
-- Thus we need a 3-place union. What we get out needs to be partitionable.
--
--   Event (Union (Union (Endo State) (JumpTo)) (Transition () t))
--
-- WOuldn't that work? esplit to get 
--
--   (Event (Union (Endo State) JumpTo), Event (Transition () t))
--
-- and then esplit again
--
--   (Event (Endo state), Event JumpTo, Event (Transition () t))
--
-- Yeah should be fine, right?
--
-- Third item: the form of stateful flows. We can't just take a behavior for
-- the state, because it will not necessarily show the latest state when the
-- next item begins! If the previous item updated on transition, then the
-- next one won't see it until a later event fires.
-- But it would be strange to ask the widgets to give a final value for the
-- state always, but also an event with endos. Also, we want them to be able
-- to hear updates from external sources...
--
--   State input must contain the changes, so let's make it a sequence?
--   This way, each individual widget can make its own state behavior should it
--   require. But, how do we come up with a state sequence? Ultimately we shall
--   give an input sequence, which shall be determined partially by the output
--   of the flow itself.
--   
{-
newtype StateR state m s t = StateR {
        getStateR :: m (s, Sequence state) (t, state)
    }

instance Arrow m => Category (StateR state m) where
    id  = StateR (arr (\(s, seqnc) -> (s, sequenceFirst seqnc)))
    StateR left . StateR right = StateR $ proc (s, seqnc) -> do
        (t, state) <- right -< (s, seqnc)
        left -< (t, state |> sequenceEvent seqnc)

instance Arrow m => Arrow (StateR state m) where
    arr f = StateR (arr (\(s, seqnc) -> (f s, sequenceFirst seqnc)))
    first (StateR sub) = StateR $ proc ((s, c), seqnc) -> do
        (t, state) <- sub -< (s, seqnc)
        returnA -< ((t, c), state)

instance ArrowChoice m => ArrowChoice (StateR state m) where
    left (StateR sub) = StateR $ proc (choice, seqnc) -> do
        case choice of
            Right c -> do returnA -< (Right c, sequenceFirst seqnc)
            Left s -> do (t, state) <- sub -< (s, seqnc)
                         returnA -< (Left t, state)

instance ArrowApply m => ArrowApply (StateR state m) where
    app = StateR $ proc ((StateR sub, s), seqnc) -> do
              app -< (sub, (s, seqnc))
-}

-- It really *should* be m (s, (Behavior state, Event state)) t
-- but we have the problem of getting the latest value on the same frame as an
-- update. Workaround maybe?
--
-- Ok so suppose we have
--
--   s :: state
--   ev :: Event (Endo state)
--
-- We can get Event state by accumE s (fmap getEndo ev)
--
--   ss :: Event state
--   ss = accumeE s (fmap getEndo ev)
--
-- And then we can get the behavior via
--
--   be :: Behavior state
--   be = stepper s ss
--
-- But of course be lags 1 frame behind ss. Can we provide for the widgets
--
--   (s, Behavior s, Event s)
--
-- by unioning with ss on changes?
-- We would require that t comes in an event...
-- But that screws with the form.
--
-- Could we just offer a Moment state to retrieving it?
-- valueB be apparently will not work; it'll be too late.
-- This is all under the assumption that some state updates will happen
-- simultaneously with transitions. Could we somehow detect when that happens
-- and ensure that we find the right value? We'd have to be able to work with
-- the transition event from outside the flow.
--
--   forall t .
--          Event (Transition d t)
--       -> Event (Transition d (t, state, Event state))
--
-- Hm but the flow continuation is already set up according to the arrow
-- instance of StateR'.
--
-- Something isn't right here. Imagine a piece which requires the user to be
-- logged in. It's not a function of the state; it needs always an auth token
-- however it may work even when there's no geolocation present.
--
--   ClosedWidget (AuthToken, Behavior (Maybe Location)) (?)
--
-- But maybe there's a map inside this widget and using it can cause the
-- location to be set. Hm...
--
-- Ok the idea is that we have an application which depends upon a fixed
-- environment (things like effectful event transformers) and also a variable
-- state. That state is recursively derived from the UI pieces. When the state
-- changes, the UI may change. However, sometimes a state change should cause
-- no effect, i.e. if a location picker updates it, we wouldn't want to reload
-- the UI... 
-- Top level is the web app flow. Only at the top level can we jumpTo.
-- Must be able to go into every single piece of the flow and add a transition
-- event according to forwards and backwards buttons.
-- Reasonable/practical to say that every piece of the flow below the web app
-- part is a reader with reactive state? We will be able to build these things
-- from other flows you know. For instance, could we qualify every sub flow
-- with a login part? If we have a flow which depends upon an auth token then
-- we can make it into a flow which does not by throwing the login part
-- in front and feeding its token through. It would then induce a state update
-- when the auth token comes out.

-- | Exactly the same as StateR!
newtype ReaderR env m s t = ReaderR {
      getReaderR :: m (s, env) t
    }

instance Arrow m => Category (ReaderR state m) where
    id  = ReaderR $ arr (\(s, _) -> s)
    ReaderR left . ReaderR right = ReaderR $ proc (s, st) -> do
        t <- right -< (s, st)
        left -< (t, st)

instance Arrow m => Arrow (ReaderR state m) where
    arr f = ReaderR $ arr (\(s, _) -> f s)
    first (ReaderR sub) = ReaderR $ proc ((s, c), st) -> do
        t <- sub -< (s, st)
        returnA -< (t, c)

instance ArrowChoice m => ArrowChoice (ReaderR state m) where
    left (ReaderR sub) = ReaderR $ proc (choice, st) -> do
        case choice of
            Right c -> do returnA -< Right c
            Left s -> do t <- sub -< (s, st)
                         returnA -< Left t

instance ArrowApply m => ArrowApply (ReaderR state m) where
    app = ReaderR $ proc ((ReaderR sub, s), st) -> do
              app -< (sub, (s, st))

runReaderR :: Arrow m => env -> ReaderR env m s t -> m s t
runReaderR env m = arr (\s -> (s, env)) >>> getReaderR m


-- | Reactive state is just a reader on reactive data. A StateR doesn't
--   update the state synchronously via its output, but must instead deliver
--   an Event which can be used lazily to define the StateRData.
type StateR state = ReaderR (StateRData state)

newtype StateRData state = StateRData {
      getStateRData :: (Behavior state, Event state)
    }

runStateR
    :: ( Arrow m )
    => state
    -> Event state
    -> StateR state m s t
    -> Moment (m s t)
runStateR st ev m = do
    be <- stepper st ev
    let d = StateRData (be, ev)
    pure $ runReaderR d m

{-
data Route (m :: * -> * -> *) s t

data WebAppM router m s t where
    WebAppEmbed :: m s t -> WebAppM route m s t
    --WebAppJump :: HasRoute router route => Proxy route -> RouteInput route -> WebAppM router m s t
    WebAppJump :: Route m s t -> WebAppM router m s t

data WebAppN n t where
    WebAppEmbedN :: n t -> WebAppN n t
    WebAppJumpN :: n t -> WebAppN n t

instance Functor n => Functor (WebAppN n) where
    fmap f (WebAppEmbedN n) = WebAppEmbedN (fmap f n)
    fmap f (WebAppJumpN n) = WebAppJumpN (fmap f n)

{-
webAppMTrans
    :: (forall s t . m s t -> n s t)
    -> WebAppM router m s t
    -> WebAppM router n s t
webAppMTrans trans (WebAppEmbed m) = WebAppEmbed (trans m)
webAppMTrans _ (WebAppJump route) = WebAppJump route
-}

dischargeWebAppM
    :: (forall s t . Route m s t -> m s t)
    -> (forall s t . m s t -> Kleisli n s t)
    -> (forall s t . WebAppM route m s t -> Kleisli (WebAppN n) s t)
dischargeWebAppM resolveRoute trans = \webAppM -> case webAppM of
    WebAppEmbed sub -> Kleisli $ \s -> WebAppEmbedN (runKleisli (trans sub) s)
    WebAppJump route -> Kleisli $ \s -> WebAppJumpN (runKleisli (trans (resolveRoute route)) s)
-}

-- Now as for web app, we may have a problem. It's intimately tied to
-- WidgetM, isn't it? How do we get the jump to event?? We have to go in and
-- alter every *widget* to push out a jump to event on certain transitions.
-- So  WebAppM (StateM (WidgetM fixed transition))  is out of the question; we
-- have to have WebAppM directly over WidgetM. This may be ok... If we come up
-- with a
--
--   WebAppM (WidgetM state transition) (s, Env env state) t
--
-- then we can wrap it in ReaderR (Env env state) to get
--
--   ReaderR (WebAppM (WidgetM state transition)) s t
--
-- ultimately yielding
--
--   env -> state -> Flow (WidgetM (Union jumpTo state) (WebAppTransition transition)) s t
--
-- or something...



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
