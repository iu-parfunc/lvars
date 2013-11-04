-- | Breadth-first search algorithms in lvish

{-# LANGUAGE CPP #-}

module Data.LVar.Graph.BFS where

import Data.Set as Set

import Utils

-- Benchmark utils:
import PBBS.FileReader
import PBBS.Timing (wait_clocks, runAndReport)
-- calibrate, measureFreq, commaint,

import Control.LVish
import Control.LVish.Internal
import Control.LVish.DeepFrz (runParThenFreezeIO)
import qualified Control.LVish.SchedIdempotent as L

import Control.Monad
import Control.Monad.Par.Combinator (parFor, InclusiveRange(..))
import Control.Monad.ST
import Control.Exception
import GHC.Conc

import Data.Word
import Data.Maybe
import Data.LVar.MaxCounter as C
import Data.Time.Clock
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Storable as UV
import qualified Data.Vector.Storable.Mutable as MV
import System.Mem (performGC)
import System.Environment (getArgs)
import System.Directory
import System.Process


-- define DEBUG_CHECKS

--------------------------------------------------------------------------------

#if 1
import Data.LVar.PureSet as S
#else
-- [2013.07.09] This one still isn't terminating on 125K+
--  Well, maybe it's just slow... 5000 takes 2 seconds.
--  Yes, it's literally over 100 times slower currently.
import Data.LVar.SLSet as S
#endif

import qualified Data.LVar.SLSet as SL

import Data.LVar.IStructure as ISt
import Data.LVar.NatArray as NArr

-- An LVar-based version of bf_traverse.  As we traverse the graph,
-- the results of applying f to each node accumulate in an LVar, where
-- they are available to other computations, enabling pipelining.
{-
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
              S.insert elem l_acc
              when dbg $ do 
                 st <- unsafePeekSet l_acc
                 prnt$ " --> Called S.insert, node "++show x
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
        fork $ S.insert (f startNode) l_acc
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
-}


--------------------------------------------------------------------------------
-- Graph algorithms
--------------------------------------------------------------------------------

bfs_async :: AdjacencyGraph -> NodeID -> Par d s (ISet s NodeID)
bfs_async gr@(AdjacencyGraph vvec evec) start = do 
  st <- S.newFromList [start]
  S.forEach st $ \ nd -> do
    logStrLn $" [bfs] expanding node "++show nd++" to nbrs " ++ show (nbrs gr nd)
    forVec (nbrs gr nd) (`S.insert` st)
  return st
--    T.traverse_ (`S.insert` st) (nbrs gr nd)


-- | A version that uses an array rather than set representation.
bfs_async_arr :: AdjacencyGraph -> NodeID -> Par d s (IStructure s Bool)
bfs_async_arr gr@(AdjacencyGraph vvec evec) start = do 
  arr <- newIStructure (U.length vvec)
  let callback nd bool = do
       let myNbrs = nbrs gr (fromIntegral nd)        
       logStrLn $" [bfs] expanding node "++show (nd,bool)++" to nbrs " ++ show myNbrs
       -- TODO: possibly use a better for loop:
       forVec myNbrs (\nbr -> ISt.put_ arr (fromIntegral nbr) True)
  ISt.forEachHP Nothing arr callback
  logStrLn $" [bfs] Seeding with start vertex... "
  ISt.put_ arr (fromIntegral start) True
  return arr

-- | Same, but with NatArray.
bfs_async_arr2 :: AdjacencyGraph -> NodeID -> Par d s (NatArray s Word8)
bfs_async_arr2 gr@(AdjacencyGraph vvec evec) start = do 
  arr <- newNatArray (U.length vvec)
  let callback nd flg = do
       let myNbrs = nbrs gr (fromIntegral nd)        
       -- logStrLn $" [bfs] expanding node "++show (nd,flg)++" to nbrs " ++ show myNbrs
       forVec myNbrs (\nbr -> NArr.put arr (fromIntegral nbr) 1)
  NArr.forEach arr callback
  -- logStrLn $" [bfs] Seeding with start vertex... "
  NArr.put arr (fromIntegral start) 1
  return arr

