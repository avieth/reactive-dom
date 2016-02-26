{-|
Module      : Reactive.DOM.Internal.Mutation
Description : Definition of Mutation, to describe DOM updates.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Reactive.DOM.Internal.Mutation where

import Prelude hiding ((.), id)
import Control.Category
import Data.Semigroup
import GHCJS.DOM.Node

-- | Formal representation of the DOM Node children mutation methods.
data ChildrenMutation t =
      AppendChild t
    | InsertBefore t t
    | ReplaceChild t t -- New then old, for consistency with the JS method.
    | RemoveChild t

deriving instance Show t => Show (ChildrenMutation t)

instance Functor ChildrenMutation where
    fmap f x = case x of
        AppendChild t -> AppendChild (f t)
        InsertBefore s t -> InsertBefore (f s) (f t)
        ReplaceChild s t -> ReplaceChild (f s) (f t)
        RemoveChild t -> RemoveChild (f t)

data SomeNode where
    SomeNode :: IsNode n => n -> SomeNode

runChildrenMutationIO
    :: IsNode node
    => ChildrenMutation SomeNode
    -> node
    -> IO ()
runChildrenMutationIO x n = action >> pure ()
  where
    action = case x of
        AppendChild (SomeNode el) -> n `appendChild` (Just el)
        InsertBefore (SomeNode el) (SomeNode el') -> insertBefore n (Just el) (Just el')
        ReplaceChild (SomeNode el) (SomeNode el') -> replaceChild n (Just el) (Just el')
        RemoveChild (SomeNode el) -> n `removeChild` (Just el)

-- | Functions f t -> g t along with ChildrenMutation lists.
--   Children container functors should come with a set of these Mutations.
newtype Mutation t f g = Mutation {
      runMutation :: f t -> (g t, [ChildrenMutation t])
    }

instance Category (Mutation t) where
    id = Mutation $ \ft -> (ft, [])
    (Mutation left) . (Mutation right) = Mutation $ \ft ->
        let (gt, cms0) = right ft
            (ht, cms1) = left gt
        in  (ht, cms1 ++ cms0)

type Automutation t f = Mutation t f f

instance Semigroup (Mutation t f f) where
    (<>) = (.)

instance Monoid (Mutation t f f) where
    mempty = Mutation $ \ft -> (ft, [])
    mappend = (<>)
