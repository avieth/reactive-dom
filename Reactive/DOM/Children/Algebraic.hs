{-|
Module      : Reactive.DOM.Children.Algebraic
Description : Algebraic composition of children.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Reactive.DOM.Children.Algebraic where

import Reactive.DOM.Internal.Mutation
import Reactive.DOM.Internal.ChildrenContainer
import Reactive.DOM.Internal.Node
import Reactive.DOM.Node (emptyWidget, openWidget)
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Data.Monoid ((<>))
import Data.Profunctor

-- | A composite children container representing the conjunction of two
--   other containers, or in other words the union of the children in both
--   containers. Its primary use case is in the definition of widgetProduct,
--   where it serves as the higher-order container (it is parameterized by
--   containers).
newtype Product (left :: (* -> *) -> *) (right :: (* -> *) -> *) inp out (f :: * -> *) = Product {
      runProduct :: (left f, right f)
    }

infixr 4 :*
type (:*) = Product

infixr 4 .*
left .* right = Product (left, right)

pattern left :* right = Product (left, right)

newtype Sum (left :: (* -> *) -> *) (right :: (* -> *) -> *) (f :: * -> *) = Sum {
      runSum :: Either (left f) (right f)
    }

instance
    ( FunctorTransformer left
    , FunctorTransformer right
    ) => FunctorTransformer (Product left right inp out)
  where
    functorTrans trans (Product (left, right)) =
        Product (functorTrans trans left, functorTrans trans right)
    functorCommute (Product (left, right)) =
        Product <$> ((,) <$> functorCommute left <*> functorCommute right)

instance
    ( FunctorTransformer left
    , FunctorTransformer right
    ) => FunctorTransformer (Sum left right)
  where
    functorTrans trans (Sum choice) = Sum (either (Left . functorTrans trans) (Right . functorTrans trans) choice)
    functorCommute (Sum choice) =
        Sum <$> either (fmap Left . functorCommute) (fmap Right . functorCommute) choice

instance
    ( ChildrenContainer left
    , ChildrenContainer right
    ) => ChildrenContainer (Product left right inp out)
  where
    type Change (Product left right inp out) =
        Sum (Change left) (Change right)
    getChange get (Sum choice) (Product (left, right)) = case choice of
        -- For changes to the left summand, we must make every AppendChild
        -- into an InsertBefore on the head of the right children, if there
        -- is a head. This ensures that the children of the left always come
        -- before the children of the right.
        Left cleft -> let (x, m) = getChange get cleft left
                          makeInsertBefore rightHead mutation = case mutation of
                              AppendChild x -> InsertBefore x rightHead
                              other -> other
                          mutations = case childrenContainerList get right of
                              [] -> m
                              first : _ -> makeInsertBefore first <$> m
                      in  (Product (x, right), mutations)
        Right cright -> let (x, m) = getChange get cright right in (Product (left, x), m)
    childrenContainerList get (Product (left, right)) =
        childrenContainerList get left ++ childrenContainerList get right

-- | Combine two widgets in such a way that their children share the same
--   DOM element. The elements of the left one come before the elements of the
--   right one in the DOM node ordering. You get both of their outputs.
widgetProduct
    :: OpenWidget s1 t1
    -> OpenWidget s2 t2
    -> OpenWidget (s1, s2) (t1, t2)
widgetProduct (Widget l1 mk1 r1) (Widget l2 mk2 r2) = Widget l mk r
  where

    l ~(pb1, pb2) ~(s1, s2) = do
        ~(pf1, r1) <- l1 pb1 s1
        ~(pf2, r2) <- l2 pb2 s2
        pure ((pf1, pf2), (r1, r2))

    r ~(pf1, pf2) ~(q1, q2) = do
      ~(pb1, t1) <- r1 pf1 q1
      ~(pb2, t2) <- r2 pf2 q2
      pure ((pb1, pb2), (t1, t2))

    mk = \(~((s1, s2), viewChildren)) -> do
        let transLeft :: forall f left right inp out . Product left right inp out f -> left f
            transLeft (~(Product (left, _))) = left
        let transRight :: forall f left right inp out . Product left right inp out f -> right f
            transRight (~(Product (_, right))) = right
        let transLeftC (~(Sum choice)) = case choice of
                Left t -> [t]
                Right _ -> []
        let transRightC (~(Sum choice)) = case choice of
                Left _ -> []
                Right t -> [t]
        let viewChildren1 = viewChildrenTrans transLeft ((=<<) transLeftC) viewChildren
        let viewChildren2 = viewChildrenTrans transRight ((=<<) transRightC) viewChildren
        -- This becomes much more complicated, no?
        -- Ah no, it doesn't have to. We just have to choose a good l and r!
        ~(t1, c1) <- mk1 (s1, viewChildren1)
        ~(t2, c2) <- mk2 (s2, viewChildren2)
        let initial = Product (childrenInitial c1, childrenInitial c2)
        -- Changes are discriminated into a Sum and list-concatenated in case of
        -- simultaneous occurrences, with changes to the left coming before changes
        -- to the right.
        let leftChanges = (fmap . fmap) (Sum . Left) (childrenChanges c1)
        let rightChanges = (fmap . fmap) (Sum . Right) (childrenChanges c2)
        let rest = unionWith (<>) leftChanges rightChanges
        pure ((t1, t2), children initial rest)

-- We need this in order to do widgetProductUniform.
-- Working with OpenWidget s t ~ forall tag . Widget tag s t in the fold just
-- doesn't seem to work. I'm not at all sure why...
data FoldOpenWidget s t where
    FoldOpenWidget :: OpenWidget s t -> FoldOpenWidget s t

widgetProductUniform
    :: forall s t f .
       ( Foldable f, Monoid t )
    => f (ClosedWidget s t)
    -> OpenWidget s t
widgetProductUniform xs =
    let FoldOpenWidget w = foldl combine' base xs
    in  w

  where

    combine' :: FoldOpenWidget s t -> ClosedWidget s t -> FoldOpenWidget s t
    combine' left right =
        let rest :: OpenWidget s t
            FoldOpenWidget rest = left
            this :: OpenWidget s t
            this = openWidget right
            combined :: OpenWidget (s, s) (t, t)
            combined = rest `widgetProduct` this
        in  FoldOpenWidget (dimap diag (uncurry (<>)) combined)

    combine :: OpenWidget s t -> ClosedWidget s t -> OpenWidget s t
    combine left right = dimap diag (uncurry (<>)) (left `widgetProduct` openWidget right)

    diag :: forall t . t -> (t, t)
    diag t = (t, t)

    base :: FoldOpenWidget s t
    base = FoldOpenWidget (rmap (const mempty) emptyWidget)

-- | Like Either but the choice is present in the type as well.
data TEither (d :: * -> Either * *) (l :: *) (r :: *) where
    TLeft :: l -> TEither 'Left l r
    TRight :: r -> TEither 'Right l r

teither :: (l -> t) -> (r -> t) -> TEither choice l r -> t
teither f g x = case x of
    TLeft l -> f l
    TRight r -> g r

tleft :: l -> TEither 'Left l r
tleft = TLeft

tright :: r -> TEither 'Right l r
tright = TRight

-- | A higher-order container representing the disjunction or two containers.
--   It is used to give the widgetSum combinator, which decides which of two
--   children containers to use based upon the input.
data DiscriminatedSum (l :: (* -> *) -> *) (r :: (* -> *) -> *) (inp :: *) (out :: *) (f :: * -> *) where
    SumLeft
        :: TEither 'Left (left f) r
        -> DiscriminatedSum left right (TEither 'Left inp a) (TEither 'Left out b) f
    SumRight
        :: TEither 'Right l (right f)
        -> DiscriminatedSum left right (TEither 'Right a inp) (TEither 'Right b out) f

instance
    ( FunctorTransformer g
    , FunctorTransformer h
    ) => FunctorTransformer (DiscriminatedSum g h (TEither choice sL sR) (TEither choice tL tR))
  where
    functorTrans trans (SumLeft (TLeft l)) = SumLeft . TLeft $ functorTrans trans l
    functorTrans trans (SumRight (TRight r)) = SumRight . TRight $ functorTrans trans r
    functorCommute (SumLeft (TLeft l)) = SumLeft . TLeft <$> functorCommute l
    functorCommute (SumRight (TRight r)) = SumRight . TRight <$> functorCommute r

instance
    ( ChildrenContainer g
    , ChildrenContainer h
    ) => ChildrenContainer (DiscriminatedSum g h (TEither choice sL sR) (TEither choice tL tR))
  where
    type Change (DiscriminatedSum g h (TEither choice sL sR) (TEither choice tL tR)) =
        DiscriminatedSum (Change g) (Change h) (TEither choice sL sR) (TEither choice tL tR)
    getChange get (SumLeft (TLeft change)) (SumLeft (TLeft l)) =
        let (new, mutations) = getChange get change l
        in  (SumLeft (TLeft new), mutations)
    getChange get (SumRight (TRight change)) (SumRight (TRight r)) =
        let (new, mutations) = getChange get change r
        in  (SumRight (TRight new), mutations)
    childrenContainerList get (SumLeft (TLeft l)) = childrenContainerList get l
    childrenContainerList get (SumRight (TRight r)) = childrenContainerList get r

-- | Combine two widgets in such a way that their children share the same
--   DOM element, but are shown in a mutually-exclusive way. If the input is
--   Left, only the left OpenWidget's children are shown. Symmetric for the
--   Right case.
widgetSum
    :: forall choice sL sR tL tR .
       OpenWidget sL tL
    -> OpenWidget sR tR
    -> OpenWidget (TEither choice sL sR) (TEither choice tL tR)
widgetSum (Widget lL mkL rL) (Widget lR mkR rR) =
    Widget (l lL lR) (mk mkL mkR) (r rL rR)

  where

    -- Step 1: construct input and output arrows, dealing with the passthrough
    -- and passback.

    l :: forall tag choice pbL pbR pfL pfR rL rR .
         (pbL -> sL -> ElementBuilder tag (pfL, rL))
      -> (pbR -> sR -> ElementBuilder tag (pfR, rR))
      -> TEither choice pbL pbR
      -> TEither choice sL sR
      -> ElementBuilder tag (TEither choice pfL pfR, TEither choice rL rR)
    l lL lR pb x = case (pb, x) of
        (leftPbL, TLeft sL) -> do
            -- It's essential that we do not force leftPbL even to its
            -- TLeft constructor. Fortunately for us, GHC will let us use a lazy
            -- match here, because the match on TLeft sL means leftPbL is
            -- indeed a TLeft.
            let (~(TLeft pbL)) = leftPbL
            ~(pfL, rL) <- lL pbL sL
            pure (TLeft pfL, TLeft rL)
        (rightPbR, TRight sR) -> do
            let (~(TRight pbR)) = rightPbR
            ~(pfR, rR) <- lR pbR sR
            pure (TRight pfR, TRight rR)

    -- The output function is free to pattern match on both parts of its
    -- input; no laziness constraints here.
    r :: forall tag choice pbL pbR pfL pfR qL qR .
         (pfL -> qL -> ElementBuilder tag (pbL, tL))
      -> (pfR -> qR -> ElementBuilder tag (pbR, tR))
      -> TEither choice pfL pfR
      -> TEither choice qL qR
      -> ElementBuilder tag (TEither choice pbL pbR, TEither choice tL tR)
    r rL rR pf x = case (pf, x) of
        (TLeft pfL, TLeft qL) -> do
            ~(pbL, tL) <- rL pfL qL
            pure (TLeft pbL, TLeft tL)
        (TRight pfR, TRight qR) -> do
            ~(pbR, tR) <- rR pfR qR
            pure (TRight pbR, TRight tR)

    -- Step 2: given the left and right intrinsic parts, come up with a
    -- discriminated intrinsic part, which decides which intrinsic part to use
    -- based upon the values (and therefore type) of the input.
    --
    -- Wow, what a type.

    mk :: forall tag fL fR rL rR qL qR .
          ((rL, ViewChildren (fL rL qL)) -> ElementBuilder tag (qL, Children (fL rL qL)))
       -> ((rR, ViewChildren (fR rR qR)) -> ElementBuilder tag (qR, Children (fR rR qR)))
       -> (forall choice .
             (TEither choice rL rR, ViewChildren (DiscriminatedSum (fL rL qL) (fR rR qR) (TEither choice rL rR) (TEither choice qL qR)))
          -> ElementBuilder tag (TEither choice qL qR, Children (DiscriminatedSum (fL rL qL) (fR rR qR) (TEither choice rL rR) (TEither choice qL qR)))
          )
    -- The fact that we pattern match on the input *may* be a deal-breaker.
    mk mkL mkR = \(~(choiceIn, viewChildren)) -> case choiceIn of
        TLeft rL -> do
            -- By pattern matching on choiceIn and finding TLeft, we also
            -- discover that the type of viewChildren must be
            --
            --   ViewChildren (DiscriminatedSum (fL rL qL) (fR rR qR) (TEither 'Left rL r) (TEither 'Left qL qR)
            --
            let viewChildrenLeft = viewChildrenTrans prjLeft (fmap prjLeft) viewChildren
            ~(qL :: qL, cL :: Children (fL rL qL)) <- mkL (rL, viewChildrenLeft)
            let setChildren = childrenTrans injLeft (fmap injLeft) cL
            pure (TLeft qL, setChildren)
        TRight rR -> do
            let viewChildrenRight = viewChildrenTrans prjRight (fmap prjRight) viewChildren
            ~(qR :: qR, cR :: Children (fR rR qR)) <- mkR (rR, viewChildrenRight)
            let setChildren = childrenTrans injRight (fmap injRight) cR
            pure (TRight qR, setChildren)

    -- Projections and injections from discriminated sum, to be used in
    -- viewChildrenTrans and childrenTrans.

    prjLeft
        :: forall g h l a b f .
           DiscriminatedSum g h (TEither 'Left l a) b f
        -> g f
    prjLeft (SumLeft (TLeft x)) = x

    prjRight
        :: forall g h r a b f .
           DiscriminatedSum g h (TEither 'Right a r) b f
        -> h f
    prjRight (SumRight (TRight x)) = x

    injLeft
        :: forall g h s t a b f .
           g f
        -> DiscriminatedSum g h (TEither 'Left s a) (TEither 'Left t b) f
    injLeft = SumLeft . TLeft

    injRight
        :: forall g h s t a b f .
           h f
        -> DiscriminatedSum g h (TEither 'Right a s) (TEither 'Right b t) f
    injRight = SumRight . TRight
