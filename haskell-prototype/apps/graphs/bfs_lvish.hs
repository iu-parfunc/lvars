{-# LANGUAGE CPP, ScopedTypeVariables #-}

import Data.Set as Set

import Util.PBBS
import Control.LVish
import Control.LVish.Internal

import Data.Word
import Data.LVar.MaxCounter as C
import Data.Traversable as T
import Data.Time.Clock
import qualified Data.Vector.Unboxed as U
import System.Mem (performGC)

#if 1
import Data.LVar.Set as S
#else 
import Data.LVar.SLSet as S
#endif

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
-}


--------------------------------------------------------------------------------
-- Graph algorithms
--------------------------------------------------------------------------------

bfs_async :: AdjacencyGraph -> NodeID -> Par d s (ISet s NodeID)
bfs_async gr@(AdjacencyGraph vvec evec) start = do 
  st <- S.newFromList [start]
  forEach st $ \ nd -> do
    logStrLn $" [bfs] expanding node "++show nd++" to nbrs " ++ show (nbrs gr nd)
    U.forM_ (nbrs gr nd) (`putInSet` st)
  return st
--    T.traverse_ (`putInSet` st) (nbrs gr nd)

maxDegree :: AdjacencyGraph -> (ISet s NodeID) -> Par d s (MaxCounter s)
maxDegree gr component = do
  mc <- newMaxCounter 0 
  forEach component $ \ nd ->
    C.put mc (U.length$ nbrs gr nd)
  return mc

-- Lattice where undecided = bot, and chosen/nbrchosen are disjoint middle states
flag_UNDECIDED :: Word8
flag_CHOSEN    :: Word8
flag_NBRCHOSEN :: Word8
flag_UNDECIDED = 0
flag_CHOSEN    = 1
flag_NBRCHOSEN = 2

-- | Uses a notion of priority writes.
-- maximalIndependentSet :: ISet s NodeID -> Par d s (ISet s NodeID)  -- Operate on a subgraph
maximalIndependentSet :: AdjacencyGraph -> Par d s (ISet s NodeID) -- Operate on a whole graph.
maximalIndependentSet = loop 0
  where
    loop self = do
      let flag = undefined
      prioWrite self flag
      undefined

    prioWrite = undefined
    
-- need to compute a fold over neighbors... some of which are bottom
-- if any neighbors are less than us and decided, we commit too..



--------------------------------------------------------------------------------
-- Main Program
--------------------------------------------------------------------------------
  
main = do
  -- let file = "../../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_10000000"
  -- let file = "../../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_125000" -- ~1sec on 1core
  -- let file = "../../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_500000"    -- ~6sec 
  let file = "../../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_1000"
  putStrLn$ "Reading file: "++file
  t0 <- getCurrentTime  
  gr <- readAdjacencyGraph file
  t1 <- getCurrentTime
  let numVerts = U.length (vertOffets gr)
  putStrLn$ "graph read ("++show (diffUTCTime t1 t0)++
    "): verts,edges: "++show (numVerts, U.length (allEdges gr))

  putStrLn$ "max vert off "++show (U.foldl1 max (vertOffets gr))
  putStrLn$ "max edge target "++show (U.foldl1 max (allEdges gr))
  t2 <- getCurrentTime
  putStrLn$ "time for those simple folds: "++show (diffUTCTime t2 t1)
  performGC
  -- writeFile "/tmp/debug" (show gr)
  -- putStrLn$ "Dumped parsed graph to /tmp/debug"
  
  let par1 :: Par d0 s0 (MaxCounter s0, ISet s0 NodeID)
      par1 = do component <- bfs_async gr 0
                liftIO$ putStrLn "Got component..."
                mc <- maxDegree gr component    
                return (mc,component)
  let -- par2 :: Par d0 s0 (ISet s0 NodeID)
      par2 :: Par d0 s0 ()
      par2 = do comp <- bfs_async gr 0
                waitSize numVerts comp -- A proxy for completeness... assumes fully connected graph.
  t0 <- getCurrentTime
#if 0               
  (maxdeg::Int, set:: Snapshot ISet NodeID) <- runParThenFreezeIO2 par1
  putStrLn$ "Processing finished, max degree was: "++show maxdeg
  let ISetSnap s = set
  putStrLn$ "Connected component, set size "++show (Set.size s)  
#else  
--  set:: Snapshot ISet NodeID <- runParThenFreezeIO par2
  _ <- runParIO par2
#endif
  t1 <- getCurrentTime
  putStrLn$ "Done"
  putStrLn$ "Time in runPar: "++show (diffUTCTime t1 t0)
  
