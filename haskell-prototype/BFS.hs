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
  putStrLn$"Begin loading graph from "++file  
  t0    <- getCurrentTime
  inStr <- B.readFile file
  let -- Returns a list of edges:
      loop1 [] = []
      loop1 (b1:b2:rst) = do
        case (B.readInt b1, B.readInt b2) of
          (Just (src,_), Just (dst,_)) -> (src,dst) : loop1 rst
          _ -> error$"Failed parse of bytestrings: "++show(B.unwords[b1,b2])
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
  -- Just to make SURE its computed:
  putStrLn$" * Graph loaded, "++show(V.length g)++" vertices, neighbors of vertex 0: "++ show (nbrs g 0)
  t1 <- getCurrentTime
  putStrLn$ " * Time reading/parsing data: "++show(diffUTCTime t1 t0)
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

type WorkRet = (Float, Float)
type WorkFn = (Float -> WorkRet)

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
  let k_ :: Int
      k_ = 25    -- Number of hops to explore
      
  let w_ :: Int
      w_ = 20000 -- Amount of work (iterations of sin)

  let graphFile_ :: String
      graphFile_ = "/tmp/grid_125000"
  
  args <- getArgs
  
  -- LK: this way of writing the type annotations is the only way I
  -- can get emacs to not think this is a parse error! :(
  let (k,w,graphFile) = 
        case args of
          []                  -> (k_, w_, graphFile_)
          [ks]                -> (read ks :: Int, w_, graphFile_)
          [ks, ws]            -> (read ks :: Int, read ws :: Int, graphFile_)
          [ks, ws, graphFile] -> (read ks :: Int, read ws :: Int, read graphFile :: String)
  
  g <- mkGraphFromFile graphFile

  let startNode = 0
      g2 = V.map IS.fromList g
  evaluate (g2 V.! 0)
  
  let graphThunk :: WorkFn -> IO ()
      graphThunk fn = do
        start_traverse k g2 0 fn
        putStrLn "Done with traversal."
  
  let sin_iter_count   :: WorkFn
      sin_iter_count x = (x, sin_iter w x)

  printf "* Beginning benchmark with k=%d and w=%d\n" k w

  performGC
  t0 <- getCurrentTime
  graphThunk sin_iter_count
  t1 <- getCurrentTime
  putStrLn $ "SELFTIMED " ++ show (diffUTCTime t1 t0)

------------------------------------------------------------------------------------------

bf_traverse :: Int             -- iteration counter
               -> Graph2       -- graph
               -> ISet WorkRet -- LVar
               -> IS.IntSet    -- set of "seen" node labels, initially size 0
               -> IS.IntSet    -- set of "new" node labels, initially size 1
               -> WorkFn       -- function to be applied to each node
               -> Par (IS.IntSet)
bf_traverse 0 _ _ seen_rank new_rank _ = do
  when verbose $ prnt $ "bf_traverse finished! seen/new size "
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
    mapM_ (\x -> fork$ do 
              let elem = f (fromIntegral x)
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
        prnt $ "Running on " ++ show numCapabilities ++ " parallel resources..."
        l_acc <- newEmptySet
        -- "manually" add startNode
        fork $ putInSet (f (fromIntegral startNode)) l_acc
        -- pass in { startNode } as the initial "new" set
        set <- bf_traverse k g l_acc IS.empty (IS.singleton startNode) f
        prnt $ "Done with bf_traverse..."
        let size = IS.size set
        prnt$ " Waiting on "++show size++" set results"

        when dbg $ do 
          forM_ [0..size] $ \ s -> do
            prnt$ " ? Blocking on "++show s++" elements to be in the set.."
            waitForSetSize s l_acc

        -- Waiting is required in any case for correctness, whether or
        -- not we consume the result
        waitForSetSize (size) l_acc -- Depends on a bunch of forked computations
        prnt$ "Set results all available! ("++show size++")"

        s <- consumeSet l_acc :: Par (Set.Set (Float,Float))
        liftIO (do evaluate s; return ())
        prnt $ "Done consume set ... "
        prnt $ "Set size: "++show (Set.size s) ++"\n "
        prnt $ "Set sum: "++show (Set.fold (\(_,x) y -> x+y) 0 s)
