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

-- | Symbolic widget flow piece.
newtype WidgetM fixed transition s t = WidgetM {
      runWidgetM :: ClosedWidget s (Event (Union fixed (transition t)))
    }

instance Functor transition => Profunctor (WidgetM fixed transition) where
    dimap l r (WidgetM cw) = WidgetM (dimap l ((fmap . fmap . fmap) r) cw)

instance Functor transition => Functor (WidgetM fixed transition s) where
    fmap = rmap

newtype WidgetN fixed transition t = WidgetN {
      runWidgetN :: UI (Event (Union fixed (transition t)))
    }

instance Functor transition => Functor (WidgetN fixed transition) where
    fmap f = WidgetN . (fmap . fmap . fmap . fmap) f . runWidgetN

transWidgetM
    :: (fixed1 -> fixed2)
    -> (forall t . transition1 t -> transition2 t)
    -> WidgetM fixed1 transition1 s t
    -> WidgetM fixed2 transition2 s t
transWidgetM transF transT (WidgetM cw) = WidgetM ((fmap . fmap) (bimap transF transT) cw)

transWidgetN
    :: (fixed1 -> fixed2)
    -> (forall t . transition1 t -> transition2 t)
    -> WidgetN fixed1 transition1 t
    -> WidgetN fixed2 transition2 t
transWidgetN transF transT (WidgetN ui) = WidgetN ((fmap . fmap) (bimap transF transT) ui)

transWidgetMN
    :: forall fixed transition .
       ( Functor transition )
    => FlowTrans (WidgetM fixed transition) (FlowKleisli (WidgetN fixed transition))
transWidgetMN (widgetM  :: WidgetM fixed transition s t) = Kleisli $ \s -> 
    let cw :: ClosedWidget s (Event (Union fixed (transition t)))
        cw = runWidgetM widgetM
        ui :: UI (Event (Union fixed (transition t)))
        ui = lmap (const s) cw
    in  FlowNext (WidgetN ((fmap . fmap . fmap . fmap) FlowDone ui))

runWidgetFlow
    :: forall fixed transition t .
       ( Functor transition )
    => (UI (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))) -> UI (Event (Union fixed (transition t))))
    -> FlowContinuation (WidgetN fixed transition) t
    -> Either t (UI (Event (Union fixed (transition t))))
runWidgetFlow _ (FlowDone t) = Left t
runWidgetFlow contractor (FlowNext next) = 
    let ui :: UI (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t))))
        ui = runWidgetN next
    in  Right (contractor ui)

-- This one, along with widgetFlow below it, exhibit some useful symmetry.
-- Run a flow down to the FlowKleisli level, pass it through runWidgetFlow',
-- then through widgetFlow, and then throw on any arrow transformers as
-- necessary. It's like openWidget/closeWidget.
runWidgetFlow'
    :: forall fixed transition s t .
       ( Functor transition )
    => (UI (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))) -> UI (Event (Union fixed (transition t))))
    -> FlowKleisli (WidgetN fixed transition) s t
    -> (s -> Either t (UI (Event (Union fixed (transition t)))))
runWidgetFlow' contractor kleisli = \s ->
    runWidgetFlow contractor (runKleisli kleisli s)

widgetFlow
    :: forall fixed transition s t .
       ( )
    => (s -> Either t (UI (Event (Union fixed (transition t)))))
    -> Flow (WidgetM fixed transition) s t
widgetFlow mk = proc s -> do
    case mk s of
        Left t -> do returnA -< t
        Right ui -> do app -< (FlowM (WidgetM ui), ())


-- | A transition is a certain kind of Functor which always determines a value
--   of its parameter type.
--   It doesn't need to be an Applicative, but it must be able to work with an
--   Either in Bitraversable style.
class Functor f => Transition (f :: * -> *) where
    splitTransition :: f (Either s t) -> Either (f s) (f t)
    runTransition :: f t -> t

instance Transition Identity where
    splitTransition = bimap Identity Identity . runIdentity
    runTransition = runIdentity

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
       ( Transition transition
       , ChildrenContainer (container (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))) () (Event (Union fixed (transition t))))
       )
    => (container (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))) () (Event (Union fixed (transition t))) Child
       -> Child (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))))
    -> (UI (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t))))
       -> container (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))) () (Event (Union fixed (transition t))) SetChild)
    -> (transition (UI (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))))
       -> [Change (container (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))) () (Event (Union fixed (transition t)))) SetChild])
    -> (forall t . Event (transition t) -> ElementBuilder tag ())
    -- ^ You can use the event of *all* transitions to alter the widget, but
    --   that event doesn't come out; only the final transition is given.
    -> UI (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t))))
    -> Widget tag () (Event (Union fixed (transition t)))
runGeneralWidgetFlow getChild setChildI setChildC builder ui = widget $

    \(_, viewChildren :: ViewChildren (container (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))) () (Event (Union fixed (transition t))))) -> do

    let first :: Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))
        first = childData . getChild $ viewChildrenInitial viewChildren
    let rest :: Event (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t))))
        rest = childData . getChild <$> viewChildrenEvent viewChildren
    continuations :: Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))
        <- sequenceSwitchE (first |> rest)

    let lefts :: Event fixed
        rights :: Event (transition (FlowContinuation (WidgetN fixed transition) t))
        (lefts, rights) = esplit continuations

    let nexts :: Event (Either (transition t) (transition (UI (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))))))
        nexts = fmap (splitTransition . fmap takeNext) rights

    let final :: Event (transition t)
        transitions :: Event (transition (UI (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t))))))
        (final, transitions) = split nexts


    let output :: Event (Union fixed (transition t))
        output = eunion lefts final

    let kids :: Children (container (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))) () (Event (Union fixed (transition t))))
        kids = children (setChildI ui)
                        (setChildC <$> transitions)

    _ <- builder rights

    pure $ (output, kids)

-- |
takeNext
    :: forall fixed transition t .
       ( )
    => FlowContinuation (WidgetN fixed transition) t
    -> Either t (UI (Event (Union fixed (transition (FlowContinuation (WidgetN fixed transition) t)))))
takeNext term = case term of
    FlowDone t -> Left t
    FlowNext next -> Right (runWidgetN next)

-- | A simple flow interpretation. The current flow piece is the sole child.
--   Old flow pieces are discarded.
runSimpleWidgetFlow
    :: forall tag fixed t .
       ( )
    => UI (Event (Union fixed (Identity (FlowContinuation (WidgetN fixed Identity) t))))
    -> Widget tag () (Event (Union fixed (Identity t)))
runSimpleWidgetFlow = runGeneralWidgetFlow getChild setChildI setChildC (const (pure ()))

  where

    getChild :: forall r s t f . Single r s t f -> f r
    getChild = runSingle

    setChildI :: forall r s t . UI r -> Single r s t SetChild
    setChildI = Single . newChild 

    setChildC :: forall r s t . Identity (UI r) -> [Change (Single r s t) SetChild]
    setChildC = pure . Single . newChild . runIdentity

-- | Run a flow using the Direction functor. New flow items will animate in
--   and out of view. It won't look right unless the widget is given good
--   width and height bounds, such that it does not grow to accomodate new
--   children.
runDirectedWidgetFlow
    :: forall tag fixed t .
       ( W3CTag tag )
    => UI (Event (Union fixed (Direction (FlowContinuation (WidgetN fixed Direction) t))))
    -> Widget tag () (Event (Union fixed (Direction t)))
runDirectedWidgetFlow =

    runGeneralWidgetFlow getChild setChildI setChildC setParentStyle

  where

    getChild :: forall f s t r . DirectedContainer Direction t s (Event r) f -> f t
    getChild (DirectedContainer (x, _)) = unDirection x

    setChildI
        :: forall s t r .
           UI (Event (Union fixed (Direction t)))
        -> DirectedContainer Direction (Event (Union fixed (Direction t))) s (Event r) SetChild
    setChildI x = DirectedContainer (newChild <$> setChildStyle (North x), Nothing)

    setChildC
        :: forall s t r .
           Direction (UI (Event (Union fixed (Direction t))))
        -> [Change (DirectedContainer Direction (Event (Union fixed (Direction t))) s (Event r)) SetChild]
    setChildC x = [DirectedChange (newChild <$> setChildStyle x)]

    parentStyleBase :: Style
    parentStyleBase = makeStyle [
          ("width", "100%")
        , ("height", "100%")
        , ("display", "flex")
        , ("overflow", "hidden")
        ]

    parentStyle :: Direction () -> Style
    parentStyle d = makeStyle [
          if isHorizontal d then ("flex-direction", "row") else ("flex-direction", "column")
        ]

    setParentStyle :: forall t . Event (Direction t) -> ElementBuilder tag ()
    setParentStyle ev = do
        style (always (Set parentStyleBase))
        style (Set . parentStyle . fmap (const ()) <$> ((North undefined) |> ev))

    setChildStyle
        :: forall r t .
           Direction (UI (Event (Union fixed (Direction t))))
        -> Direction (UI (Event (Union fixed (Direction t))))
    setChildStyle term = case term of
        North cw -> North $ setChildStyle' (North ()) cw
        South cw -> South $ setChildStyle' (South ()) cw
        East cw -> East $ setChildStyle' (East ()) cw
        West cw -> West $ setChildStyle' (West ()) cw

    setChildStyle'
        :: forall t .
           Direction ()
        -> UI (Event (Union fixed (Direction t)))
        -> UI (Event (Union fixed (Direction t)))
    setChildStyle' d (ClosedWidget tag w) = ClosedWidget tag (w `modifyr` childModifier d)

    childModifier
        :: forall t tag .
           ( W3CTag tag )
        => Direction ()
        -> Modifier tag (Event (Union fixed (Direction t))) (Event (Union fixed (Direction t)))
    childModifier d = modifier (makeTransition d)

    makeTransition
        :: forall t tag .
           ( W3CTag tag )
        => Direction ()
        -> Event (Union fixed (Direction t))
        -> ElementBuilder tag (Event (Union fixed (Direction t)))
    makeTransition d ev = do
        let ev' = eright ev
        animFrame <- requestAnimationFrame
        let styleOn = const (Set (onStyle d)) <$> animFrame
        let styleOff = Set . offStyle <$> ev'
        control <- stepper False (const True <$> ev')
        let styleChange = unionWith const styleOff (whenE (not <$> control) styleOn)
        style (Set (initialStyle d) |> styleChange)
        pure ev
    
    initialStyle :: forall t . Direction t -> Style
    initialStyle d = makeStyle [
          ("transition", "all 1s")
        , ("-webkit-transition", "all 1s")
        , if isHorizontal d then ("width", "0px") else ("height", "0px")
        , ("opacity", "0")
        ]

    onStyle :: forall t . Direction t -> Style
    onStyle d = makeStyle [
          ("transition", "all 1s")
        , ("-webkit-transition", "all 1s")
        , ("width", "100%")
        , ("height", "100%")
        , ("opacity", "1")
        ]

    offStyle :: forall t . Direction t -> Style
    offStyle d = makeStyle [
          ("transition", "all 1s")
        , ("-webkit-transition", "all 1s")
        , if isHorizontal d then ("width", "0px") else ("height", "0px")
        , ("opacity", "0")
        ]


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


data Direction t =
      North t
    | South t
    | East t
    | West t

unDirection :: Direction t -> t
unDirection (North t) = t
unDirection (South t) = t
unDirection (East t) = t
unDirection (West t) = t

isHorizontal :: Direction t -> Bool
isHorizontal (West _) = True
isHorizontal (East _) = True
isHorizontal _ = False

isVertical :: Direction t -> Bool
isVertical = not . isHorizontal

instance Functor Direction where
    fmap f term = case term of
        North t -> North (f t)
        South t -> South (f t)
        East t -> East (f t)
        West t -> West (f t)

instance Transition Direction where
    runTransition = unDirection
    splitTransition term = case term of
        North t -> bimap North North t
        South t -> bimap South South t
        East t -> bimap East East t
        West t -> bimap West West t

instance Foldable Direction where
    foldr f b term = case term of
        North t -> f t b
        South t -> f t b
        East t -> f t b
        West t -> f t b

instance Traversable Direction where
    traverse f term = case term of
        North m -> North <$> f m
        South m -> South <$> f m
        East m -> East <$> f m
        West m -> West <$> f m

newtype DirectedContainer (d :: * -> *) r s t f = DirectedContainer {
      runDirectedContainer :: (d (f r), Maybe (f r))
    }

newtype DirectedChange d r s t f = DirectedChange {
      runDirectedChange :: d (f r)
    }

instance Traversable d => FunctorTransformer (DirectedContainer d r s t) where
    functorTrans f (DirectedContainer (dx, my)) = DirectedContainer (fmap f dx, fmap f my)
    functorCommute (DirectedContainer (mx, mmy)) =
        let x = sequenceA (getCompose <$> mx)
            my = sequenceA (getCompose <$> mmy)
        in  DirectedContainer <$> ((,) <$> x <*> my)

instance Traversable d => FunctorTransformer (DirectedChange d r s t) where
    functorTrans f (DirectedChange d) = DirectedChange (fmap f d)
    functorCommute (DirectedChange md) =
        let x = sequenceA (getCompose <$> md)
        in  DirectedChange <$> x

instance ChildrenContainer (DirectedContainer Direction r s t) where

    type Change (DirectedContainer Direction r s t) = DirectedChange Direction r s t

    getChange get (DirectedChange change) (DirectedContainer (x, my)) =
        let removals = case my of
                Nothing -> []
                Just old -> [RemoveChild (get old)]
            additions = case change of
                North it -> [AppendChild (get it)]
                South it -> [InsertBefore (get it) (get (unDirection x))]
                East it -> [AppendChild (get it)]
                West it -> [InsertBefore (get it) (get (unDirection x))]
        in  (DirectedContainer (change, Just (unDirection x)), additions ++ removals)

    childrenContainerList get (DirectedContainer (x, my)) =
        let other = case my of
                Nothing -> []
                Just y -> [get y]
        in  case x of
                North it -> other ++ [get it]
                South it -> [get it] ++ other
                East it -> other ++ [get it]
                West it -> [get it] ++ other
