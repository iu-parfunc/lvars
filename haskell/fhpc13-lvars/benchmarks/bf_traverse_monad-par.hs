{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import           Control.Monad (when)
import qualified Data.Set as Set
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import           GHC.Conc (numCapabilities)

import           Control.Monad.Par (Par, runParIO)
import           Control.Monad.Par.Combinator (parMap, parMapM, parFor, InclusiveRange(..))
import           Debug.Trace (trace)

import Runner

prnt :: String -> Par ()
prnt str = trace str $ return ()

-- This version of bf_traverse is based on monad-par, but it doesn't
-- accumulate results in an LVar while traversing the graph, as
-- BF_LVar does.  Instead, it does the whole traversal, then maps f
-- over the resulting set of nodes once the traversal has finished.
-- (Note that bf_traverse does not even take f as an argument, as it
-- does in BFS_LVar.)

bf_traverse :: Int             -- iteration counter
               -> Graph2       -- graph
               -> IS.IntSet    -- set of "seen" node labels, initially size 0
               -> IS.IntSet    -- set of "new" node labels, initially size 1
               -> Par (IS.IntSet)
bf_traverse 0 _ seen_rank new_rank = do
  when verbose $ prnt $ "bf_traverse finished! seen/new size: "
    ++ show (IS.size seen_rank, IS.size new_rank)
  return (IS.union seen_rank new_rank)

bf_traverse k !g !seen_rank !new_rank = do 
  when verbose $ prnt  $"bf_traverse call... "
    ++ show k ++ " seen/new size "
    ++ show (IS.size seen_rank, IS.size new_rank)
  -- Nothing in the new_rank set means nothing left to traverse.
  if IS.null new_rank
  then return seen_rank
  else do
    -- Add new_rank stuff to the "seen" list
    let seen_rank' = IS.union seen_rank new_rank
        allNbr'    = IS.fold (\i acc -> IS.union (g V.! i) acc) 
                        IS.empty new_rank
        new_rank'  = IS.difference allNbr' seen_rank' 
    bf_traverse (k-1) g  seen_rank' new_rank'

start_traverse :: Int       -- iteration counter
                  -> Graph2 -- graph
                  -> Int    -- start node
                  -> WorkFn -- function to be applied to each node
                  -> IO ()
start_traverse k !g startNode f = do
  runParIO $ do        
        prnt $ " * Running on " ++ show numCapabilities ++ " parallel resources..."
        
        -- pass in { startNode } as the initial "new" set
        set <- bf_traverse k g IS.empty (IS.singleton startNode)
        
        prnt $ " * Done with bf_traverse..."

        resLs <- parMap f (IS.toList set)
        let set2 = Set.fromList resLs

        prnt $ " * Done parMap(f)..."
        prnt $ "  * Set size: " ++ show (Set.size set2)
        prnt $ "  * Set sum: " ++ show (Set.fold (\(x,_) y -> x+y) 0 set2)

main = makeMain start_traverse
