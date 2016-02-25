{-|
Module      : Reactive.DOM.Edit
Description : A tool to compute minimal list edits.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.DOM.Edit where

import Data.Array
import Data.Monoid 

-- | Edits without swaps.
data Edit s t = Drop s | Cons t | Keep s t
  deriving Show

mapEdit :: (s -> s') -> (t -> t') -> Edit s t -> Edit s' t'
mapEdit f g edit = case edit of
    Drop s -> Drop (f s)
    Cons t -> Cons (g t)
    Keep s t -> Keep (f s) (g t)

editCost :: Edit s t -> Int
editCost edit = case edit of
    Drop _ -> 1
    Cons _ -> 1
    Keep _ _ -> 0

-- | Effectful reconstruction of lists from edits.
data RunEdit m s t r = RunEdit {
      runDrop :: s -> m ()
    , runCons :: t -> m r
    , runKeep :: s -> t -> m r
    }

runEdit :: Applicative m => RunEdit m s t r -> Edit s t -> m [r] -> m [r]
runEdit actions edit rest = case edit of
    Drop s -> flip const <$> runDrop actions s <*> rest
    Cons t -> (:) <$> runCons actions t <*> rest
    Keep s t -> (:) <$> runKeep actions s t <*> rest

runEdits :: Applicative m => RunEdit m s t r -> [Edit s t] -> m [r]
runEdits actions = foldr (runEdit actions) (pure [])

-- In practice we shall use an edit list to induce effects. For each Cons
-- we must append to the DOM and for each Drop we must remove.
-- The edit list will be computed between two [ElementSchemaChild]. Thus
-- we will obtain an
--
--     Event [Edit ElementSchemaChild]
--
-- and we must use this along with a Behavior [RenderedNode] to come up with
-- an Event [RenderedNode] which itself defines that behavior. So ultimately
-- we need
--
--     [RenderedNode] -> [Edit ElementSchemaChild] -> MomentIO [RenderedNode]
--
-- A Cons elementSchemaChild must induce a renderElementSchemaChild to produce
-- a MomentIO RenderedNode
-- A Drop must induce an unrenderNode
--
-- Ok with a slight change we can now come up with
--
--     Event [Edit RenderedNode ElementSchemaChild]
--
-- and by choosing an appropriate
--
--     RunEdit MomentIO RenderedNode ElementSchemaChild RenderedNode
--
-- we can turn this into
--
--     Event (MomentIO [RenderedNode])
--
-- and then feed that back into the earlier event. Cool!

-- | A list of edits and its cost. The Eq and Ord instances consider only
--   the length; two @EditList t@s of equal length are considered equal.
data EditList s t = EditList {
      editList :: ![Edit s t]
    , totalCost :: !Int
    }
  deriving (Show)

instance Eq (EditList s t) where
    left == right = totalCost left == totalCost right

instance Ord (EditList s t) where
    left `compare` right = totalCost left `compare` totalCost right

emptyEditList :: EditList s t
emptyEditList = EditList [] 0

editListFromNil :: [t] -> Int -> EditList s t
editListFromNil ts prefix = EditList (Cons <$> take prefix ts) prefix

editListToNil :: [s] -> Int -> EditList s t
editListToNil ts prefix = EditList (Drop <$> take prefix ts) prefix

newEdit :: Edit s t -> EditList s t -> EditList s t
newEdit edit elist = elist {
      editList = edit : editList elist
    , totalCost = editCost edit + totalCost elist
    }

-- | Edits to go from xs to ys.
--   Adaptation of the program found here:
--   https://wiki.haskell.org/Edit_distance
edits :: forall s t . (s -> t -> Bool) -> [s] -> [t] -> EditList s t
edits eq xs ys = table ! (m,n)
  where

    m :: Int
    n :: Int
    (m, n) = (length xs, length ys)

    -- Make arrays of xs and ys, for quick lookup.
    axs :: Array Int s
    axs = array (1,m) (zip [1..] xs)
    ays :: Array Int t
    ays = array (1,n) (zip [1..] ys)

    -- Our table is row, column indexed. Imagine axs lain out on the left-hand
    -- side from top to bottom, and ays on top from left to right. (0, 0) is
    -- the top-left corner.
    table :: Array (Int, Int) (EditList s t)
    table = array bnds [(ij, dist ij) | ij <- range bnds]

    bnds :: ((Int, Int), (Int, Int))
    bnds = ((0,0),(m,n))

    dist :: (Int, Int) -> EditList s t
    -- Top row is all @Cons@s.
    dist (0,j) = editListFromNil ys j
    -- Left column is all @Drop@s.
    dist (i,0) = editListToNil xs i
    dist (i,j) = minimum [
          newEdit (Cons (ays ! j)) (table ! (i,j-1))
          -- Previous row element (up) corresponds to a drop.
        , newEdit (Drop (axs ! i)) (table ! (i-1,j))
          -- Previous column element (left) corresponds to a cons.
          -- If these points are equal, no change.
          -- up and to the left (no change).
          -- Otherwise, 
        , if (axs ! i) `eq` (ays ! j)
          then newEdit (Keep (axs ! i) (ays ! j)) (table ! (i-1,j-1))
          -- NB a swap has cost 2.
          else newEdit (Drop (axs ! i)) (newEdit (Cons (ays ! j)) (table ! (i-1,j-1)))
        ]
