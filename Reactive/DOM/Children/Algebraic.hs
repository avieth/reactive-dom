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

module Reactive.DOM.Children.Algebraic where

import Reactive.DOM.Internal.Mutation
import Reactive.DOM.Internal.ChildrenContainer
import Reactive.DOM.Internal.Node
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Data.Monoid ((<>))

-- A composite children container
newtype Product (left :: (* -> *) -> *) (right :: (* -> *) -> *) (f :: * -> *) = Product {
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

infixr 3 :+
type (:+) = Sum

-- TODO injection type class for sums.

instance
    ( FunctorTransformer left
    , FunctorTransformer right
    ) => FunctorTransformer (Product left right)
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
    ) => ChildrenContainer (Product left right)
  where
    type Change (Product left right) =
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

-- This isn't quite right. Must give a way to switch from one summand to
-- the other.
instance
    ( ChildrenContainer left
    , ChildrenContainer right
    ) => ChildrenContainer (Sum left right)
  where
    type Change (Sum left right) =
        Product (Change left) (Change right)
    getChange get (Product (cleft, cright)) (Sum choice) = case choice of
        Left left -> let (x, m) = getChange get cleft left in (Sum (Left x), m)
        Right right -> let (x, m) = getChange get cright right in (Sum (Right x), m)
    childrenContainerList get (Sum choice) =
        either (childrenContainerList get)
               (childrenContainerList get)
               choice

-- | Combine two widgets in such a way that their children share the same
--   DOM element. The elements of the left one come before the elements of the
--   right one in the DOM node ordering. You get both of their outputs.
widgetProduct :: Widget s1 t1 -> Widget s2 t2 -> Widget (s1, s2) (t1, t2)
widgetProduct (Widget mk1) (Widget mk2) = widget $ \(~((s1, s2), viewChildren)) -> do
    let transLeft :: forall f left right . Product left right f -> left f
        transLeft (~(Product (left, _))) = left
    let transRight :: forall f left right . Product left right f -> right f
        transRight (~(Product (_, right))) = right
    let transLeftC (~(Sum choice)) = case choice of
            Left t -> [t]
            Right _ -> []
    let transRightC (~(Sum choice)) = case choice of
            Left _ -> []
            Right t -> [t]
    let viewChildren1 = viewChildrenTrans transLeft ((=<<) transLeftC) viewChildren
    let viewChildren2 = viewChildrenTrans transRight ((=<<) transRightC) viewChildren
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
