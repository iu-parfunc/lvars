{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Data.Set as Set

import Util.PBBS
import Control.LVish
import Control.LVish.Internal
import Control.Monad
import Control.Monad.Par.Combinator (parFor, InclusiveRange(..))

import Data.Word
import Data.LVar.MaxCounter as C
import Data.Traversable as T
import Data.Time.Clock
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as UV
import System.Mem (performGC)
import System.Environment (getArgs)
import System.Directory
import System.Process

-- define DEBUG_CHECKS

--------------------------------------------------------------------------------

#if 1
import Data.LVar.Set as S
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
  S.forEach st $ \ nd -> do
    logStrLn $" [bfs] expanding node "++show nd++" to nbrs " ++ show (nbrs gr nd)
    forVec (nbrs gr nd) (`putInSet` st)
  return st
--    T.traverse_ (`putInSet` st) (nbrs gr nd)


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

maxDegree :: AdjacencyGraph -> (ISet s NodeID) -> Par d s (MaxCounter s)
maxDegree gr component = do
  mc <- newMaxCounter 0 
  S.forEach component $ \ nd ->
    C.put mc (U.length$ nbrs gr nd)
  return mc

-- Lattice where undecided = bot, and chosen/nbrchosen are disjoint middle states
flag_UNDECIDED :: Word8
flag_CHOSEN    :: Word8
flag_NBRCHOSEN :: Word8
flag_UNDECIDED = 0
flag_CHOSEN    = 1
flag_NBRCHOSEN = 2

{-# INLINE maximalIndependentSet #-}
-- | Uses a notion of priority writes.
-- maximalIndependentSet :: ISet s NodeID -> Par d s (ISet s NodeID)  -- Operate on a subgraph
-- maximalIndependentSet :: AdjacencyGraph -> Par d s (ISet s NodeID) -- Operate on a whole graph.
maximalIndependentSet :: ParFor d s -> AdjacencyGraph -> Par d s (NatArray s Word8) -- Operate on a whole graph.
maximalIndependentSet parFor gr@(AdjacencyGraph vvec evec) = do
  logStrLn$ " [MIS] Beginning maximalIndependentSet "
  -- For each vertex, we record whether it is CHOSEN, not chosen, or undecided:
  let numVerts = U.length vvec
  flagsArr :: NatArray s Word8 <- newNatArray numVerts
  let       
      -- Here's the loop that scans through the neighbors of a node.
      loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          -- logStrLn$ " [MIS]   ... on nbr "++ show i++" of "++show numNbrs
          let nbrInd = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          -- If we got to the end of the neighbors below us, then we are NOT disqualified:
          if nbrInd > selfInd
            then thisNodeWins
            else do
              -- This should never block in a single-thread execution:
              logStrLn (" [MIS] ! Getting on nbrInd "++show nbrInd)
              nbrFlag <- NArr.get flagsArr (fromIntegral nbrInd)
              logStrLn (" [MIS] ! Get completed on nbrInd "++show nbrInd)
              if nbrFlag == flag_CHOSEN
                then NArr.put flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = logStrLn (" [MIS] ! Node chosen: "++show selfInd) >> 
                         NArr.put flagsArr (fromIntegral selfInd) flag_CHOSEN
  parFor (0,numVerts) $ \ ndIx -> do 
      let nds = nbrs gr (fromIntegral ndIx)
      -- logStrLn$ " [MIS] processing node "++show ndIx++" nbrs "++show nds
      loop (U.length nds) nds ndIx  0
  return flagsArr

  
-- need to compute a fold over neighbors... some of which are bottom
-- if any neighbors are less than us and decided, we commit too..


--------------------------------------------------------------------------------
-- Misc helpers
--------------------------------------------------------------------------------

{-# INLINE forVec #-}
-- | Simple for-each loops over vector elements.
forVec :: U.Unbox a => U.Vector a -> (a -> Par d s ()) -> Par d s ()
forVec vec fn = loop 0 
  where
    len = U.length vec
    loop i | i == len = return ()
           | otherwise = fn (U.unsafeIndex vec i) >>
                         loop (i+1)

type ParFor d s = (Int,Int) -> (Int -> Par d s ()) -> Par d s ()

--------------------------------------------------------------------------------
-- Main Program
--------------------------------------------------------------------------------
  
main = do
  putStrLn "USAGE: ./bfs_lvish <version> <graphSize>"

  args <- getArgs
  let (version,size) = case args of
                         [n,s] -> (read n, read s)
                         [n]   -> (read n, 1000)
                         _     -> (3,1000)

  let root = "../../../pbbs/breadthFirstSearch/graphData/data/"
      file = "3Dgrid_J_"++show size
  -- let file = "../../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_10000000"
  -- let file = "../../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_500000"    -- ~6sec
  -- let file = "../../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_125000" -- ~1sec on 1core
  -- let file = "../../../pbbs/breadthFirstSearch/graphData/data/3Dgrid_J_1000"

  origdir <- getCurrentDirectory
  setCurrentDirectory root  
  b <- doesFileExist file
  unless b $ do
    putStrLn "Input file does not exist!  Building..."
    system$ "make "++file
    return ()
  
  ------------------------------------------------------------  
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
  
  t0 <- getCurrentTime
  case version of
    ----------------------------------------
    1 -> do putStrLn " ! Version 1: work in progress testing combinations of graph ops..."
            let par1 :: Par d0 s0 (MaxCounter s0, ISet s0 NodeID)
                par1 = do component <- bfs_async gr 0
                          liftIO$ putStrLn "Got component..."
                          mc <- maxDegree gr component    
                          return (mc,component)            
            (maxdeg::Int, set:: Snapshot ISet NodeID) <- runParThenFreezeIO2 par1
            putStrLn$ "Processing finished, max degree was: "++show maxdeg
            let ISetSnap s = set
            putStrLn$ "Connected component, set size "++show (Set.size s)

    ----------------------------------------
    2 -> do putStrLn " ! Version 2: BFS only, with sets "
            let -- par2 :: Par d0 s0 (ISet s0 NodeID)
                -- par2 :: Par d0 s0 ()
                par2 = do comp <- bfs_async gr 0
                          waitSize numVerts comp -- A proxy for completeness... assumes fully connected graph.
                          return comp
            _ <- runParIO_ par2
            -- set:: Snapshot ISet NodeID <- runParThenFreezeIO par2
            -- let ISetSnap s = set                                          
            -- putStrLn$ "Connected component, set size "++show (Set.size s)
            return ()
            
    ----------------------------------------
    3 -> do putStrLn " ! Version 3: BFS only, with IStructures "
            let -- par2 :: Par d0 s0 (ISet s0 NodeID)
                par3 :: Par d0 s0 (IStructure s0 Bool)
                par3 = bfs_async_arr gr 0
            --  set:: Snapshot ISet NodeID <- runParThenFreezeIO par2
            _ <- runParIO_ par3
            return ()

    ----------------------------------------
    4 -> do putStrLn " ! Version 4: BFS only, with NatArrays "
            let -- par2 :: Par d0 s0 (ISet s0 NodeID)
                par4 :: Par d0 s0 (NatArray s0 Word8)
                par4 = bfs_async_arr2 gr 0
            --  set:: Snapshot ISet NodeID <- runParThenFreezeIO par2
            _ <- runParIO_ par4
            return ()


    ----------------------------------------
    5 -> do putStrLn " ! Version 5: MIS only, with NatArrays / parForSimple"
            let par :: Par d0 s0 (NatArray s0 Word8)
                par = maximalIndependentSet parForSimple gr
#ifdef DEBUG_CHECKS
            NatArraySnap (x :: UV.Vector Word8) <- runParThenFreezeIO par
            putStrLn$ "MIS: result prefix: "++show (UV.take 100 x)
            putStrLn$ "MIS: number of vertices in result: "++show (UV.sum (UV.filter (==1) x))
#else
            _ <- runParIO_ par
#endif
            return ()

    ----------------------------------------
    6 -> do putStrLn " ! Version 6: MIS only, with NatArrays / parForTree"
            let par :: Par d0 s0 (NatArray s0 Word8)
                par = maximalIndependentSet parForTree gr
            _ <- runParIO_ par
            return ()

    ----------------------------------------
    7 -> do putStrLn " ! Version 7: MIS only, with NatArrays / parForL"
            let par :: Par d0 s0 (NatArray s0 Word8)
                par = maximalIndependentSet parForL gr
            _ <- runParIO_ par
            return ()




  t1 <- getCurrentTime
  putStrLn$ "Done"
  putStrLn$ "SELFTIMED: "++show (diffUTCTime t1 t0)
  


