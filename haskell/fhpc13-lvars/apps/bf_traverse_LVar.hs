{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import           Control.Exception (evaluate)
import           Control.Monad (forM_, when)
import qualified Data.Set as Set
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import           GHC.Conc (numCapabilities)

import qualified Control.Monad.Par as Par
import           Control.Monad.Par.Combinator (parMap, parMapM, parFor, InclusiveRange(..))
import           LVarTracePure
import           Data.LVar.SetPure 
import           Debug.Trace (trace)

import Runner

prnt :: String -> Par ()
prnt str = trace str $ return ()

-- An LVar-based version of bf_traverse.  As we traverse the graph,
-- the results of applying f to each node accumulate in an LVar, where
-- they are available to other computations, enabling pipelining.

bf_traverse :: Int             -- iteration counter
               -> Graph2       -- graph
               -> ISet WorkRet -- LVar
               -> IS.IntSet    -- set of "seen" node labels, initially size 0
               -> IS.IntSet    -- set of "new" node labels, initially size 1
               -> WorkFn       -- function to be applied to each node
               -> Par (IS.IntSet)
bf_traverse 0 _ _ seen_rank new_rank _ = do
  when verbose $ prnt $ "bf_traverse finished! seen/new size: "
    ++ show (IS.size seen_rank, IS.size new_rank)
  return (IS.union seen_rank new_rank)

bf_traverse k !g !l_acc !seen_rank !new_rank !f = do 
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

    -- We COULD use callbacks here, but rather we're modeling what happens in the
    -- current paper:
    parMapM_ (\x -> fork$ do 
              let elem = f x
              putInSet elem l_acc
              when dbg $ do 
                 st <- unsafePeekSet l_acc
                 prnt$ " --> Called putInSet, node "++show x
                      ++" size is "++show(Set.size st) 
                      ++" elem is "++show elem --  ++" "++show st
            )
            (IS.toList new_rank') -- toList is HORRIBLE
    bf_traverse (k-1) g l_acc seen_rank' new_rank' f

start_traverse :: Int       -- iteration counter
                  -> Graph2 -- graph
                  -> Int    -- start node
                  -> WorkFn -- function to be applied to each node
                  -> IO ()
start_traverse k !g startNode f = do
  runParIO $ do        
        prnt $ " * Running on " ++ show numCapabilities ++ " parallel resources..."
        
        l_acc <- newEmptySet
        -- "manually" add startNode
        fork $ putInSet (f startNode) l_acc
        -- pass in { startNode } as the initial "new" set
        set <- bf_traverse k g l_acc IS.empty (IS.singleton startNode) f
        
        prnt $ " * Done with bf_traverse..."
        let size = IS.size set
        
        prnt$ " * Waiting on "++show size++" set results..."

        when dbg $ do 
          forM_ [0..size] $ \ s -> do
            prnt$ " ? Blocking on "++show s++" elements to be in the set..."
            waitForSetSize s l_acc

        -- Waiting is required in any case for correctness, whether or
        -- not we consume the result
        waitForSetSize (size) l_acc -- Depends on a bunch of forked computations
        prnt$ " * Set results all available! (" ++ show size ++ ")"

        s <- consumeSet l_acc :: Par (Set.Set WorkRet)
        liftIO (do evaluate s; return ())
        prnt $ " * Finished consumeSet:"
        prnt $ "  * Set size: " ++ show (Set.size s)
        prnt $ "  * Set sum: " ++ show (Set.fold (\(x,_) y -> x+y) 0 s)

parMapM_ f l =
  do parMapM f l
     return ()

main = makeMain start_traverse

