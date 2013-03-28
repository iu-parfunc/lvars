{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- Compile-time options:
--   PURE        -- use LVarTracePure
--
-- Run-time options:
--   W = work to do per vertex
--   K = max hops of the connected component to explore
--   (OR N = target vertices to visit (will overshoot))

#ifdef PURE
#warning "Using the PURE version"
import LVarTracePure
#else
import LVarTraceIO
#endif

import           Control.DeepSeq (deepseq)
import           Control.Exception (evaluate)
import           Control.Monad (forM_, when)
import           Control.Monad.Par.Combinator (parMap, parMapM, parFor, InclusiveRange(..))
import           Control.Monad.Par.Class (ParFuture)
import           Control.DeepSeq         (NFData)
import           Data.List as L
import qualified Data.Set as Set
import qualified Data.IntSet as IS
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Traversable (Traversable)
import           Data.Map as Map (toList, fromListWith)
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
import           GHC.Conc (numCapabilities)
import           Text.Printf (printf)
import           System.Mem (performGC)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Environment (getEnvironment,getArgs)

import qualified Control.Parallel.Strategies as Strat

-- For parsing the file produced by pbbs
import Data.List.Split (splitOn)
import System.IO (openFile, hGetContents, IOMode(ReadMode))

-- For printing inside Par
import Debug.Trace (trace)

-- For representing graphs
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

--------------------------------------------------------------------------------

-- Vector representation of graphs: the list (or set) at index k is
-- node k's neighbors.
type Graph = V.Vector [Int]
type Graph2 = V.Vector IS.IntSet

-- Optimized version:
mkGraphFromFile :: String -> IO Graph
mkGraphFromFile file = do
  putStrLn $ "* Begin loading graph from " ++ file ++ "..." 
  t0    <- getCurrentTime
  inStr <- B.readFile file
  let -- Returns a list of edges:
      loop1 [] = []
      loop1 (b1:b2:rst) = do
        case (B.readInt b1, B.readInt b2) of
          (Just (src, _), Just (dst, _)) -> (src, dst) : loop1 rst
          _ -> error $ "Failed parse of bytestrings: " ++ show (B.unwords[b1, b2])
      loop1 _ = error "Odd number of integers in graph file!"

  let edges = case B.words inStr of
               ("EdgeArray":rst) -> loop1 rst
      mx = foldl' (\mx (s,d) -> mx `max` s `max` d) 0 edges
  mg <- MV.replicate (mx+1) []
  forM_ edges $ \ (src,dst) -> do
    -- Interpret this as a DIRECTED graph:    
    ls <- MV.read mg src
    MV.write mg src (dst:ls)
  g <- V.freeze mg
  -- Just to make SURE it's computed:
  putStrLn $ " * Graph loaded: " 
    ++ show(V.length g) ++ " vertices.  Neighbors of vertex 0: "
    ++ show (nbrs g 0)
  t1 <- getCurrentTime
  putStrLn $ " * Time reading/parsing data: " ++ show(diffUTCTime t1 t0)
  return g

-- Neighbors of a node with a given label
nbrs :: Graph -> Int -> [Int]
nbrs g lbl = g V.! lbl

-- For debugging
printGraph :: Graph -> IO ()
printGraph g = do
  let ls = V.toList g
  putStrLn (show ls)
  return ()
    
-- Iterates the sin function n times on its input and returns the sum
-- of all iterations.
sin_iter :: Int -> Float -> Float
sin_iter 0  x = x
sin_iter n !x = sin_iter (n - 1) (x + sin x)

type WorkRet = (Float, Int)
type WorkFn = (Int -> WorkRet)

prnt :: String -> Par ()
prnt str = trace str $ return ()

theEnv :: [(String,String)]
theEnv = unsafePerformIO getEnvironment

checkEnv :: Read a => String -> a -> a 
checkEnv v def =
  case lookup v theEnv of
    Just "" -> def
    Just s  -> read s    
    Nothing -> def

verbose :: Bool
verbose = checkEnv "VERBOSE" False

dbg :: Bool
-- dbg = checkEnv "DEBUG" False
dbg = False -- Let it inline, DCE.

main :: IO ()
main = do
  -- Fetch runtime parameters:
  -- First, defaults:
  let graphFile_ :: String
      graphFile_ = "/tmp/grid_125000"
  
  let k_ :: Int
      k_ = 25    -- Number of hops to explore
      
  let w_ :: Int
      w_ = 20000 -- Amount of work (iterations of sin)

  args <- getArgs
  
-- LK: this way of writing the type annotations is the only way I
  -- can get emacs to not think this is a parse error! :(
  let (graphFile,k,w) = 
        case args of
          []                   -> (graphFile_, k_, w_)
          [graphFiles]         -> (graphFiles, k_, w_)
          [graphFiles, ks]     -> (graphFiles, read ks :: Int, w_)
          [graphFiles, ks, ws] -> (graphFiles, read ks :: Int, read ws :: Int)
  
  g <- mkGraphFromFile graphFile

  let startNode = 0
      g2 = V.map IS.fromList g
  evaluate (g2 V.! 0)
  
  let graphThunk :: WorkFn -> IO ()
      graphThunk fn = do
        start_traverse k g2 0 fn
        putStrLn "All done."
  
  let sin_iter_count :: WorkFn
      sin_iter_count x = let f = fromIntegral x in
                         (sin_iter w f, x)

  printf "* Beginning benchmark with k=%d and w=%d\n" k w

  performGC
  t0 <- getCurrentTime
  graphThunk sin_iter_count
  t1 <- getCurrentTime
  putStrLn $ "SELFTIMED " ++ show (diffUTCTime t1 t0) ++ "\n"

------------------------------------------------------------------------------------------



