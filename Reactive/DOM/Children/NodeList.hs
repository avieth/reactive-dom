{-|
Module      : Reactive.DOM.Children.NodeList
Description : Arbitrary list of nodes.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.DOM.Children.NodeList where

import Reactive.DOM.Internal.Mutation
import Reactive.DOM.Children.Cardinality
import Data.Maybe (mapMaybe)
import Data.Array
import Data.Monoid 

-- | Relatively unstructured children, like you would be working with if you
--   chose raw JavaScript: there's a list of children, and no other static
--   information.
newtype NodeList t = NodeList {
      runNodeList :: [t]
    }

-- | Move to a NodeList from 0 children.
nodeList :: Eq t => [t] -> Mutation t (Cardinality Zero) NodeList
nodeList ts = Mutation $ \_ -> (NodeList ts, runEditList (edits (==) [] ts))

-- | Use a new list of nodes. The required DOM mutations are computed using
--   a dynamic programming algorithm adapted from string edit distance. This
--   is why the Eq constraint is here. In practice, it will be JavaScript
--   reference equality on DOM Nodes.
nodeListSet :: Eq t => [t] -> Automutation t NodeList
nodeListSet ts = Mutation $ \(NodeList olds) ->
    (NodeList ts, runEditList (edits (==) olds ts))

-- | Edits without swaps. No change contains new and then old, for consistency
--   with ChildrenMutation constructors. If you come accross a NoChange x y
--   then x and y should be equal in some sense (according to the parameter
--   of the function edits).
data Edit t = Change (ChildrenMutation t) | NoChange t t
  deriving Show

editCost :: Edit t -> Int
editCost edit = case edit of
    Change _ -> 1
    NoChange _ _ -> 0

runEdits :: [Edit t] -> [ChildrenMutation t]
runEdits = mapMaybe pickEdit
  where
    pickEdit (Change x) = Just x
    pickEdit (NoChange _ _) = Nothing

-- | A list of edits and its cost. The Eq and Ord instances consider only
--   the length; two @EditList t@s of equal length are considered equal.
data EditList t = EditList {
      editList :: ![Edit t]
    , totalCost :: !Int
    }
  deriving (Show)

instance Eq (EditList t) where
    left == right = totalCost left == totalCost right

instance Ord (EditList t) where
    left `compare` right = totalCost left `compare` totalCost right

emptyEditList :: EditList t
emptyEditList = EditList [] 0

editListFromNil :: [t] -> Int -> EditList t
editListFromNil ts prefix = EditList (Change . AppendChild <$> take prefix ts) prefix

editListToNil :: [t] -> Int -> EditList t
editListToNil ts prefix = EditList (Change . RemoveChild <$> take prefix ts) prefix

change :: ChildrenMutation t -> EditList t -> EditList t
change mutation elist = elist {
      editList = edit : editList elist
    , totalCost = editCost edit + totalCost elist
    }
  where
    edit = Change mutation

noChange :: t -> t -> EditList t -> EditList t
noChange t1 t2 elist = elist {
      editList = edit : editList elist
    , totalCost = editCost edit + totalCost elist
    }
  where
    edit = NoChange t1 t2

runEditList :: EditList t -> [ChildrenMutation t]
runEditList = runEdits . editList

-- | Edits (DOM mutations) to go from xs to ys.
--   Adaptation of the program found here:
--   https://wiki.haskell.org/Edit_distance
edits :: forall t . (t -> t -> Bool) -> [t] -> [t] -> EditList t
edits eq xs ys = table ! (m,n)

  where

    m :: Int
    n :: Int
    (m, n) = (length xs, length ys)

    -- Make arrays of xs and ys, for quick lookup.
    -- We reverse them because we need to build up the table in such a way
    -- that later (as in appearing later in the DOM Node list) edits are known
    -- before earlier ones, so that we can come up with a reference for
    -- InsertBefore.
    axs :: Array Int t
    axs = array (1,m) (zip [1..] (reverse xs))
    ays :: Array Int t
    ays = array (1,n) (zip [1..] (reverse ys))

    -- Our table is row, column indexed. Imagine axs lain out on the left-hand
    -- side from top to bottom, and ays on top from left to right. (0, 0) is
    -- the top-left corner.
    table :: Array (Int, Int) (EditList t)
    table = array bnds [(ij, dist ij) | ij <- range bnds]

    bnds :: ((Int, Int), (Int, Int))
    bnds = ((0,0),(m,n))

    dist :: (Int, Int) -> EditList t
    dist (0,j) = editListFromNil ys j
    dist (i,0) = editListToNil xs i
    dist (i,j) = minimum [
          -- Previous column element (left) in any row but the 0'th gives an
          -- InsertBefore.
          -- We know that if i /= 0, j > 0, then (i,j-1) is a nonempty edit
          -- list whose first element is anything but Change (AppendChild t)
          -- because AppendChild only appears in the 0'th row.
          let prev = table ! (i,j-1)
              before = case prev of
                  elist@(EditList (l : ls) cost) -> case l of
                      Change (InsertBefore _ x) -> x
                      Change (ReplaceChild _ x) -> x
                      Change (RemoveChild x) -> x
                      NoChange _ x -> x
          in  change (InsertBefore (ays ! j) before) (table ! (i,j-1))
          -- Previous row element (up) corresponds to a drop.
        , change (RemoveChild (axs ! i)) (table ! (i-1,j))
          -- If these points are equal, no change. Otherwise we'll
          -- replace the old with new.
        , if (axs ! i) `eq` (ays ! j)
          then noChange (ays ! j) (axs ! i) (table ! (i-1,j-1))
          else change (ReplaceChild (ays ! j) (axs ! i)) (table ! (i-1,j-1))
        ]
