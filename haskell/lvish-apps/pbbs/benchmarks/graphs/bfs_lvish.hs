{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Data.Set as Set

import Util.PBBS
import Control.LVish
import Control.LVish.Internal

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

import Util

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

------------------------------------------------------------------------------------------
-- A simple FOLD operation.
------------------------------------------------------------------------------------------  

maxDegreeS :: AdjacencyGraph -> (ISet s NodeID) -> Par d s (MaxCounter s)
maxDegreeS gr component = do
  mc <- newMaxCounter 0 
  S.forEach component $ \ nd ->
    C.put mc (U.length$ nbrs gr nd)
  return mc


maxDegreeN :: AdjacencyGraph -> (NatArray s Word8) -> Par d s (MaxCounter s)
maxDegreeN gr component = do
  mc <- newMaxCounter 0 
  NArr.forEach component $ \ nd flg ->
    when (flg == 1) $
      C.put mc (U.length$ nbrs gr (fromIntegral nd))
  return mc


maxDegreeI :: AdjacencyGraph -> (IStructure s Word8) -> Par d s (MaxCounter s)
maxDegreeI gr component = do
  mc <- newMaxCounter 0
  -- INEFFICIENT: this attaches a handler to ALL ivars:
  ISt.forEachHP Nothing component $ \ nd flg -> do
    when (flg == 1) $ do
      let degree = U.length$ nbrs gr (fromIntegral nd)
      -- logStrLn$ " [maxDegreeI] Processing: "++show(nd,flg)++" with degree "++show degree
      C.put mc degree

  -- Better to just do this... wait for it to freeze and then loop.
  -- Problem is, we need to add wait-till-frozen!
  -- len <- ISt.getLength component
  -- parForTiled (0,len) $ \ nd ->
  --   when (flg == 1) $
  --     C.put mc (U.length$ nbrs gr (fromIntegral nd))

  return mc

------------------------------------------------------------------------------------------
-- A dummy per-node operation
------------------------------------------------------------------------------------------  

-- workEachNode :: (NatArray s Word8) -> (Word8 -> Par d s ()) -> Par d s (MaxCounter s)
workEachNode :: Word64 -> (NatArray s Word8) -> Par d s ()
workEachNode clocks component = do
  NArr.forEach component $ \ nd flg ->
    when (flg == 1) $ do
      liftIO$ wait_clocks clocks
      return ()

-- After freezing... this is a parallel loop, but doesn't use any monotonic data.
workEachVec :: Word64 -> UV.Vector Word8 -> Par d s ()
workEachVec clocks vec = do
  np <- liftIO$ getNumCapabilities
  -- for_ (0,UV.length vec) $ \ ix ->    
  -- parForTiled (np*4) (0,UV.length vec) $ \ ix ->
  -- parForSimple (0,UV.length vec) $ \ ix ->
  parForTree (0,UV.length vec) $ \ ix ->  
    let flg = vec UV.! ix in
    when (flg == 1) $ do
      liftIO$ wait_clocks clocks
      return ()

  -- Sequential version:  
  -- UV.forM_ vec $ \ flg ->
  --   when (flg == 1) $ do
  --     liftIO$ wait_clocks clocks
  --     return ()


workEachNodeI :: Word64 -> (IStructure s Word8) -> Par d s ()
workEachNodeI clocks component = do
  ISt.forEachHP Nothing component $ \ nd flg ->
    when (flg == 1) $ do
      liftIO$ wait_clocks clocks
      return ()

-- After freezing... this is a parallel loop, but doesn't use any monotonic data.
workEachVecMayb :: Word64 -> V.Vector (Maybe Word8) -> Par d s ()
workEachVecMayb clocks vec = do
  np <- liftIO$ getNumCapabilities
  -- for_ (0,UV.length vec) $ \ ix ->    
  -- parForTiled (np*4) (0,UV.length vec) $ \ ix ->
  -- parForSimple (0,UV.length vec) $ \ ix ->
  parForTree (0, V.length vec) $ \ ix ->  
    let flg = vec V.! ix in
    when (flg == Just 1) $ do
      liftIO$ wait_clocks clocks
      return ()


------------------------------------------------------------------------------------------
-- Maximal Independent Set
------------------------------------------------------------------------------------------  

-- Lattice where undecided = bot, and chosen/nbrchosen are disjoint middle states
flag_UNDECIDED :: Word8
flag_CHOSEN    :: Word8
flag_NBRCHOSEN :: Word8
flag_UNDECIDED = 0
flag_CHOSEN    = 1
flag_NBRCHOSEN = 2

{-# INLINE maximalIndependentSet #-}
-- maximalIndependentSet :: ISet s NodeID -> Par d s (ISet s NodeID)  -- Operate on a subgraph
-- maximalIndependentSet :: AdjacencyGraph -> Par d s (ISet s NodeID) -- Operate on a whole graph.
maximalIndependentSet :: ParFor d s -> AdjacencyGraph -> Par d s (NatArray s Word8) -- Operate on a whole graph.
maximalIndependentSet parFor gr@(AdjacencyGraph vvec evec) = do
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

-- | DUPLICATE CODE: IStructure version.
maximalIndependentSet2 :: ParFor d s -> AdjacencyGraph -> Par d s (IStructure s Word8) -- Operate on a whole graph.
maximalIndependentSet2 parFor gr@(AdjacencyGraph vvec evec) = do
  logStrLn$ " [MIS] Beginning maximalIndependentSet / Istructures"
  -- For each vertex, we record whether it is CHOSEN, not chosen, or undecided:
  let numVerts = U.length vvec
  flagsArr <- newIStructure numVerts
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
              nbrFlag <- ISt.get flagsArr (fromIntegral nbrInd)
              logStrLn (" [MIS] ! Get completed on nbrInd "++show nbrInd)
              if nbrFlag == flag_CHOSEN
                then ISt.put_ flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = logStrLn (" [MIS] ! Node chosen: "++show selfInd) >> 
                         ISt.put_ flagsArr (fromIntegral selfInd) flag_CHOSEN
  parFor (0,numVerts) $ \ ndIx -> do 
      let nds = nbrs gr (fromIntegral ndIx)
      -- logStrLn$ " [MIS] processing node "++show ndIx++" nbrs "++show nds
      loop (U.length nds) nds ndIx  0
  return flagsArr


-- | Sequential version.
maximalIndependentSet3 :: AdjacencyGraph -> (U.Vector Word8)
maximalIndependentSet3 gr@(AdjacencyGraph vvec evec) = U.create $ do
  let numVerts = U.length vvec
  flagsArr <- M.replicate numVerts 0
  let loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          let nbrInd   = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          if nbrInd > selfInd
            then thisNodeWins
            else do
              nbrFlag <- M.read flagsArr (fromIntegral nbrInd)
              if nbrFlag == flag_CHOSEN
                then M.write flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = M.write flagsArr (fromIntegral selfInd) flag_CHOSEN
  for_ (0,numVerts) $ \ ndIx -> do 
      let nds = nbrs gr (fromIntegral ndIx)
      loop (U.length nds) nds ndIx 0
  return flagsArr

-- | Sequential version on NatArray...
maximalIndependentSet3B :: AdjacencyGraph -> (UV.Vector Word8) -> (UV.Vector Word8)
maximalIndependentSet3B gr@(AdjacencyGraph vvec evec) vec = UV.create $ do
  let numVerts = U.length vvec
  flagsArr <- MV.replicate numVerts 0
  let loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          let nbrInd   = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          if nbrInd > selfInd
            then thisNodeWins
            else do
              nbrFlag <- MV.read flagsArr (fromIntegral nbrInd)
              if nbrFlag == flag_CHOSEN
                then MV.write flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = MV.write flagsArr (fromIntegral selfInd) flag_CHOSEN
  for_ (0,numVerts) $ \ ndIx -> 
      when (vec UV.! ndIx == 1) $ do 
        let nds = nbrs gr (fromIntegral ndIx)
        loop (U.length nds) nds ndIx 0
  return flagsArr


-- MIS over a preexisting, filtered subgraph
------------------------------------------------------------
-- Right now this uses an IStructure because it's (temporarily) better at blocking gets:
maximalIndependentSet4 :: AdjacencyGraph -> (NatArray s Word8) -> Par d s (IStructure s Word8)
maximalIndependentSet4 gr@(AdjacencyGraph vvec evec) vertSubset = do
  let numVerts = U.length vvec
  -- Tradeoff: we use storage proportional to the ENTIRE graph.  If the subset is
  -- very small, this is silly and we could use a sparse representation:
  flagsArr <- newIStructure numVerts
  let       
      -- Here's the loop that scans through the neighbors of a node.
      loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          let nbrInd   = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          -- If we got to the end of the neighbors below us, then we are NOT disqualified:
          if nbrInd > selfInd
            then thisNodeWins
            else do
              nbrFlag <- ISt.get flagsArr (fromIntegral nbrInd)
              if nbrFlag == flag_CHOSEN
                then ISt.put_ flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = ISt.put_ flagsArr (fromIntegral selfInd) flag_CHOSEN
          
  NArr.forEach vertSubset $ \ ndIx _ -> 
        let nds = nbrs gr (fromIntegral ndIx) in
        loop (U.length nds) nds ndIx  0
  return flagsArr

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
  putStrLn "USAGE: ./bfs_lvish <version> <topo> <graphSize>"
  putStrLn "USAGE:   Topo must be one of: grid rmat rand chain"
  putStrLn "USAGE:   Version must be one of: "
  putStrLn "USAGE:      bfsS bfsN bfsI"
  putStrLn "USAGE:      misN1 misN2 misN3 misI3 misSeq" 
  
  --------------------------------------------------------------------------------
  args <- getArgs
  let (version,topo,size,wrksize::Double) =
        case args of
          [ver,tp,s,w] -> (ver, tp, read s, read w)
          [ver,tp,s]   -> (ver, tp, read s, 0)
          [ver,tp]     -> (ver, tp,      1000, 0)
          [ver]        -> (ver, "grid",  1000, 0) 
          []           -> ("bfsN","grid",1000, 0)
          oth          -> error "Too many command line args!"
      existD d = do b <- doesDirectoryExist d
                    return$ if b then (Just d) else Nothing
                    
  -- Here's a silly hack to let this executable run from different working directories:
  pbbsdirs <- fmap catMaybes $ mapM existD [ "../pbbs"
                                           , "../../pbbs"
                                           , "../../../pbbs"
                                           , "../../../../pbbs"]
  let pbbsroot = case pbbsdirs of
                   [] -> error "PBBS dir not found!  Is the submodule checked out?"
                   hd:_ -> hd
      datroot = pbbsroot++"/breadthFirstSearch/graphData/data/"
      -- The PBBS Makefile knowns how to build the common graphs:
      buildPBBSdat file = do 
        origdir <- getCurrentDirectory
        setCurrentDirectory datroot  
        b <- doesFileExist file
        unless b $ do
          putStrLn "Input file does not exist!  Building..."
          system$ "make "++file
          return ()
        setCurrentDirectory origdir

  file <- case topo of
           "grid" -> do let f = "3Dgrid_J_"++show size
                        buildPBBSdat f
                        return (datroot ++ f)
           -- Models social-network graphs:
           "rmat" -> do let f = "rMatGraph_J_5_"++show size
                        buildPBBSdat f
                        return (datroot ++ f)
           "rand" -> do let f = "randLocalGraph_J_5_"++show size
                        buildPBBSdat f
                        return (datroot ++ f)
           "chain" ->  do let f = "chain_"++show size
                              p = datroot ++ f
                          b <- doesFileExist p
                          unless b $ do
                            putStrLn$"Generating chain graph in "++p
                            system "ghc -threaded gen_chains_graph.hs -o ./gen_chains_graph.exe"
                            system$ "./gen_chains_graph.exe "++show size++" > "++p
                            return ()
                          return p                          
           _        -> error$"Unknown graph topology: "++topo
           
  putStrLn$"Running config: "++show(version,topo,size)
  ------------------------------------------------------------
  wd <- getCurrentDirectory
  putStrLn$ "Working dir: "++wd
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


  runAndReport $ \ clocks_per_micro ->
    let amountWork = (round (wrksize * clocks_per_micro)) in
    case version of
      ----------------------------------------
      "bfsS" -> do 
                   putStrLn " ! Version 2: BFS only, with sets "
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
      "bfsI" -> do putStrLn " ! Version 3: BFS only, with IStructures "
                   let -- par2 :: Par d0 s0 (ISet s0 NodeID)
                       par3 :: Par d0 s0 (IStructure s0 Bool)
                       par3 = bfs_async_arr gr 0
                   --  set:: Snapshot ISet NodeID <- runParThenFreezeIO par2
                   _ <- runParIO_ par3
                   return ()

      ----------------------------------------
      "bfsN" -> do putStrLn " ! Version 4: BFS only, with NatArrays "
                   let -- par2 :: Par d0 s0 (ISet s0 NodeID)
                       par4 :: Par d0 s0 (NatArray s0 Word8)
                       par4 = bfs_async_arr2 gr 0
                   --  set:: Snapshot ISet NodeID <- runParThenFreezeIO par2
                   _ <- runParIO_ par4
                   return ()

      ----------------------------------------
      "misN1" -> do 
              putStrLn " ! Version 5: MIS only, with NatArrays / parForSimple"
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
      "misN2" -> do 
              putStrLn " ! Version 6: MIS only, with NatArrays / parForTree"
              let par :: Par d0 s0 (NatArray s0 Word8)
                  par = maximalIndependentSet parForTree gr
              _ <- runParIO_ par
              return ()

      ----------------------------------------
      "misN3" -> do 
              putStrLn " ! Version 7: MIS only, with NatArrays / parForL"
              let par :: Par d0 s0 (NatArray s0 Word8)
                  par = maximalIndependentSet parForL gr
              _ <- runParIO_ par
              return ()

      ----------------------------------------
      "misI3" -> do 
              putStrLn " ! Version 8: MIS only, with IStructures / parForL"
              let par :: Par d0 s0 (IStructure s0 Word8)
                  par = maximalIndependentSet2 parForL gr
              _ <- runParIO_ par
              return ()
      -- This version doesn't get the horrible parallel slowdown of version 5-7.
      -- But alas, version 7 sequential is better.
      -- And version 9 sequential is WAY better (>50X faster)

      ----------------------------------------
      "misSeq" -> do 
              putStrLn " ! Version 9: MIS only, sequential"
              evaluate $ maximalIndependentSet3 gr
              return ()


      ----------------------------------------
      "bfsN_misI" -> do 
              putStrLn " ! Version 10: BFS and then MIS w/ NatArrays/IStructure"
              let par :: Par d0 s0 (IStructure s0 Word8)
                  par = do natarr <- bfs_async_arr2 gr 0
                           maximalIndependentSet4 gr natarr
              _ <- runParIO_ par
              return ()

      ----------------------------------------
      "bfsN_misI_deg" -> do 
              putStrLn " ! Version 11: BFS, MIS, and maxDegree"
              let par :: Par d0 s0 (MaxCounter s0)
                  par = do natarr <- bfs_async_arr2 gr 0
                           narr2 <- maximalIndependentSet4 gr natarr
                           maxDegreeI gr narr2
              mx <- runParThenFreezeIO par
              putStrLn$ "Max degree in MIS was: "++show(mx::Int)


      ----------------------------------------
      "bfsN_work" -> do
              putStrLn " ! Version 12: BFS and per-vertex work"
              let par :: Par d0 s0 ()
                  par = do natarr <- bfs_async_arr2 gr 0
                           workEachNode amountWork natarr
              _ <- runParIO_ par
              return ()

      ----------------------------------------
      "bfsN_barrier_work" -> do
              putStrLn " ! Version 13: BFS, barrier, and per-vertex work"
              let -- par :: Par d0 s0 ()
                  par = bfs_async_arr2 gr 0
              NatArraySnap vec <- runParThenFreezeIO par
              runParIO_ $ workEachVec amountWork vec
              return ()


      ----------------------------------------
      "misI_work" -> do
              putStrLn " ! Version 14: MIS and per-vertex work"
              let par :: Par d0 s0 ()
                  par = do istrct <- maximalIndependentSet2 parForL gr
                           workEachNodeI amountWork istrct
              _ <- runParIO_ par
              return ()

      ----------------------------------------
      "misI_barrier_work" -> do
              putStrLn " ! Version 15: "
              let -- par :: Par d0 s0 ()
                  par = maximalIndependentSet2 parForL gr 
              IStructSnap vec <- runParThenFreezeIO par
              runParIO_ $ workEachVecMayb amountWork vec
              return ()

      ----------------------------------------
      "bfsN_misI_work" -> do
              putStrLn " ! Version 16: "
              let par :: Par d0 s0 ()
                  par = do natarr <- bfs_async_arr2 gr 0
                           istrct <- maximalIndependentSet4 gr natarr
                           workEachNodeI amountWork istrct
              _ <- runParIO_ par
              return ()

      ----------------------------------------
      "bfsN_barrier_misI_work" -> do
              putStrLn " ! Version 17: "
              let par = bfs_async_arr2 gr 0
              NatArraySnap vec <- runParThenFreezeIO par
              let vec2 = maximalIndependentSet3B gr vec -- Sequential
              runParIO_ $ workEachVec amountWork vec2
              return ()

      ----------------------------------------
      "?" -> do
              putStrLn " ! Version 1: work in progress testing combinations of graph ops..."
              let par1 :: Par d0 s0 (MaxCounter s0, ISet s0 NodeID)
                  par1 = do component <- bfs_async gr 0
                            liftIO$ putStrLn "Got component..."
                            mc <- maxDegreeS gr component    
                            return (mc,component)            
              (maxdeg::Int, set:: Snapshot ISet NodeID) <- runParThenFreezeIO2 par1
              putStrLn$ "Processing finished, max degree was: "++show maxdeg
              let ISetSnap s = set
              putStrLn$ "Connected component, set size "++show (Set.size s)

      oth -> error$"Unknown benchmark mode "++oth

  putStrLn$ "Done"
  


