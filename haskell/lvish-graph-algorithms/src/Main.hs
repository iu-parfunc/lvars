{-# LANGUAGE CPP, ScopedTypeVariables #-}

import Data.Set as Set

import Utils
import Data.LVar.Graph.BFS
import Data.LVar.Graph.MIS

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
                   _ <- runParIO_ par3
                   return ()

      ----------------------------------------
      "bfsN" -> do putStrLn " ! Version 4: BFS only, with NatArrays "
                   let -- par2 :: Par d0 s0 (ISet s0 NodeID)
                       par4 :: Par d0 s0 (NatArray s0 Word8)
                       par4 = bfs_async_arr2 gr 0
                   _ <- runParIO_ par4
                   return ()
{-
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
-}
      oth -> error$"Unknown benchmark mode "++oth

  putStrLn$ "Done"
  

-- runParIO_ :: (forall s . Par d s a) -> IO ()
-- runParIO_ p = do runParIO p; return ()

-- Unsafe version, fix this:
runParIO_ :: (Par d s a) -> IO ()
runParIO_ (WrapPar p) = L.runParIO p >> return ()

