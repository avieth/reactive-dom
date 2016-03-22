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
{-# LANGUAGE OverloadedStrings #-}

module Reactive.DOM.Internal.Mutation where

import Prelude hiding ((.), id)
import Data.Text (pack)
import Control.Category
import Data.Semigroup
import Data.Bifunctor
import GHCJS.DOM.Node
import GHCJS.DOM.Types (Text)
import GHCJS.DOM.Element (setAttribute, IsElement)
import Data.JSString.Text
import Data.Unique

-- | Formal representation of the DOM Node children mutation methods.
data ChildrenMutation old new =
      AppendChild new
    | InsertBefore new old
    | ReplaceChild new old -- New then old, for consistency with the JS method.
    | RemoveChild old

deriving instance (Show old, Show new) => Show (ChildrenMutation old new)

instance Bifunctor ChildrenMutation where
    bimap f g x = case x of
        AppendChild new -> AppendChild (g new)
        InsertBefore new old -> InsertBefore (g new) (f old)
        ReplaceChild new old -> ReplaceChild (g new) (f old)
        RemoveChild old -> RemoveChild (f old)

data SomeNode where
    SomeNode :: IsNode n => Unique -> n -> SomeNode

instance Show SomeNode where
    show (SomeNode u _) = show (hashUnique u)

instance Eq SomeNode where
    (SomeNode u1 _) == (SomeNode u2 _) = u1 == u2

instance Ord SomeNode where
    (SomeNode u1 _) `compare` (SomeNode u2 _) = u1 `compare` u2

someText :: Text -> IO SomeNode
someText node = do
    u <- newUnique
    pure (SomeNode u node)

{-# NOINLINE someElement #-}
someElement :: IsElement e => e -> IO SomeNode
someElement node = do
    u <- newUnique
    let hash = hashUnique u
    setAttribute node (textToJSString "virtual_id") (textToJSString (pack (show hash)))
    pure (SomeNode u node)

runChildrenMutationIO
    :: IsNode parent
    => ChildrenMutation SomeNode SomeNode
    -> parent
    -> IO ()
runChildrenMutationIO x parent = action >> pure ()
  where
    action = case x of
        AppendChild (SomeNode _ el) -> parent `appendChild` (Just el)
        InsertBefore (SomeNode _ new) (SomeNode _ old) ->
            insertBefore parent (Just new) (Just old)
        ReplaceChild (SomeNode _ new) (SomeNode _ old) ->
            replaceChild parent (Just new) (Just old)
        RemoveChild (SomeNode _ el) -> parent `removeChild` (Just el)

runChildrenMutationsIO
    :: IsNode parent
    => [ChildrenMutation SomeNode SomeNode]
    -> parent
    -> IO ()
runChildrenMutationsIO ms parent =
    traverse (flip runChildrenMutationIO parent) ms >> pure ()
