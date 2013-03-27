-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- Compile time options:
--   PURE        -- use LVarTracePure
--   NOCRITERION -- turn off criterion and do one big run
--
-- Run-time options:
--   W = work to do per vertex
--   K = max hops of the connected component to explore
--   (OR N = target vertices to visit (will overshoot))
--   VER = 1 or 2

#ifdef PURE
#warning "Using the PURE version"
import LVarTracePure
#else
import LVarTraceInternal 
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

-- So we can see the behavior of bf_traverse
import Debug.Trace (trace)

-- For representing graphs
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

#ifndef NOCRITERION
-- For benchmarking
import Criterion.Main ()
import Criterion.Config ()
myConfig :: Config
myConfig = defaultConfig {
  cfgSamples = ljust 10,
  cfgPerformGC = ljust True
}
#endif
--------------------------------------------------------------------------------


-- Vector representation of graphs: the list at index k is the list of
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
            -- let (ls,mx) = loop1 rst in
            -- ((src,dst):ls, mx `max` src `max` dst)
          _ -> error$"Failed parse of bytestrings: "++show(B.unwords[b1,b2])
      loop1 _ = error "Odd number of integers in graph file!"
--  g <- MV.new 
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
  putStrLn$" * Graph loaded, "++show(V.length g)++" vertices, first vertex: "++ show (nbrs g 0)
  t1 <- getCurrentTime
  putStrLn$ " * Time reading/parsing data: "++show(diffUTCTime t1 t0)
  return g


-- Neighbors of a node with a given label
nbrs :: Graph -> Int -> [Int]
nbrs g lbl = g V.! lbl

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

type WorkFn = (Float -> (Float,Float))

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

main :: IO ()
main = do
  -- Fetch runtime parameters:
  -- First, defaults:
  let k_,w_ :: Int
--      n_ = 1000  -- Number of nodes.
      k_ = 25    -- Number of hops to explore
      w_ = 20000 -- Amount of work (sin's)
  
  args <- getArgs
  -- TODO: this needs to use more sophisticated argument parsing if it will coexist
  -- with criterion.  Or could use env vars instead.
  let k,w,ver :: Int
      (k,w,ver) = 
        case args of
          []      -> (k_,w_,2)
          [ks]    -> (read ks,w_,2)
          [ks,ws] -> (read ks,read ws,2)
          [ks,ws,ver] -> (read ks,read ws,read ver)
  
--  g <- mkGraphFromFile "/tmp/grid_1000"
--  g <- mkGraphFromFile "/tmp/grid_8000"
  g <- mkGraphFromFile "/tmp/grid"

  let startNode = 0
      g2 = V.map IS.fromList g
  evaluate (g2 V.! 0)
  printf "Using VER %d of the BFS code...\n" ver
  
  let graphThunk :: WorkFn -> IO ()
      graphThunk fn = do case ver of
                           1 -> start_traverse  k g  0 fn
                           2 -> start_traverse2 k g2 0 fn
                         putStrLn "Done with traversal."
  let sin_iter_count x = (x,sin_iter w x)

  printf " * Begining benchmark with k=%d and w=%d\n" k w
#ifdef NOCRITERION
  performGC
  t0 <- getCurrentTime
  graphThunk sin_iter_count
  t1 <- getCurrentTime
  putStrLn$"SELFTIMED "++show (diffUTCTime t1 t0)
#else
  defaultMainWith myConfig (return ()) [
         bgroup "bf_traverse" [
           bench "sin_iter" $ graphThunk sin_iter_count
         ]
         ]
#endif

------------------------------------------------------------------------------------------

-- Takes a graph, a start node, and a function to be applied to each
-- node.
start_traverse :: Int -> Graph -> Int -> WorkFn -> IO ()
start_traverse k !g startNode f = do
  runParIO $ do
        prnt $ "Running on " ++ show numCapabilities ++ " parallel resources..."
        l_acc <- newEmptySet
        -- "manually" add startNode
        fork$ putInSet (f (fromIntegral startNode)) l_acc
        result <- bf_traverse k g l_acc Set.empty (Set.singleton startNode) f
        prnt $ "Evaling result..."
        liftIO (do evaluate result; return ())

        -- ERROR: This is our classic data-race... need to wait first.
        
        prnt $ "Done with bf_traverse... "
        s <- consumeSet l_acc :: Par (Set.Set (Float,Float))
        liftIO (do evaluate s; return ()) -- this explodes
        prnt $ "Done consume set ... "
        return ()

bf_traverse :: Int -> Graph -> ISet (Float,Float) -> Set.Set Int -> Set.Set Int -> WorkFn
               -> Par (Set.Set Int)
bf_traverse 0 _ _ seen _ _ = return seen
bf_traverse k !g !l_acc !seen_rank !new_rank !f = do 
  trace ("bf_traverse call.. "++show k++" seen/new size "
         ++show (Set.size seen_rank, Set.size new_rank)) $ return () 
  
  -- Nothing in the new_rank set means nothing left to traverse.
  if Set.null new_rank
  then return seen_rank
  else do
    -- Add new_rank stuff to the "seen" list
    let seen_rank' =  Set.union seen_rank new_rank
    -- Add to the next rank, and to the output/accumulator:
    let add :: Int -> Par (Set.Set Int)
        add n = if Set.member n seen_rank'
                then return Set.empty
                else do fork $ putInSet (f (fromIntegral n)) l_acc
                        return (Set.singleton n)
    -- Grab the neighbor nodes of everything in the new_rank set.
    let parMapMAdd :: [Int] -> Par [Set.Set Int]
        parMapMAdd = myMapM add 
    let getNeighbors = parMapMAdd . (nbrs g) :: Int -> Par [Set.Set Int]
    new_rank' <- myMapM getNeighbors (Set.toList new_rank)
    
    -- Flatten it out, this should be a parallel fold ideally:
    let new_rank'' = Set.unions $ concat new_rank'
    bf_traverse (k-1) g l_acc seen_rank' new_rank'' f

-- Takes a graph, an LVar, a set of "seen" node labels, a set of "new"
-- node labels, and the function f to be applied to each node.
bf_traverse2 :: Int -> Graph2 -> ISet (Float,Float) -> IS.IntSet -> IS.IntSet -> WorkFn
               -> Par (IS.IntSet)
bf_traverse2 0 _ _ seen_rank new_rank _ = do
  when verbose (prnt ("bf_traverse2 END..  seen/new size "
                      ++show (IS.size seen_rank, IS.size new_rank)))
  return (IS.union seen_rank new_rank)
bf_traverse2 k !g !l_acc !seen_rank !new_rank !f = do 
  when verbose (prnt ("bf_traverse2 call.. "++show k++" seen/new size "
                      ++show (IS.size seen_rank, IS.size new_rank)))
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
    myMapM_ (\x -> do 
              -- st <- unsafePeekSet l_acc
              -- prnt$ " --> Calling putInSet, "++show x
              --     ++" size is "++show(Set.size$ st)
              fork$ putInSet (f (fromIntegral x)) l_acc)
            (IS.toList new_rank') -- toList is HORRIBLE
    bf_traverse2 (k-1) g l_acc seen_rank' new_rank' f


-- Takes a graph, a start node, and a function to be applied to each
-- node.
start_traverse2 :: Int -> Graph2 -> Int -> WorkFn -> IO ()
start_traverse2 k !g startNode f = do
  runParIO $ do        
        prnt $ "Running on " ++ show numCapabilities ++ " parallel resources..."
        l_acc <- newEmptySet
        -- "manually" add startNode
        fork $ putInSet (f (fromIntegral startNode)) l_acc
        set <- bf_traverse2 k g l_acc IS.empty (IS.singleton startNode) f
        prnt $ "Done with bf_traverse..."
        let size = IS.size set
        prnt$ " Waiting on "++show size++" set results"
        
        -- Actually, waiting is required in any case for correctness...
        -- whether or not we consume the result:
        waitForSetSize (size) l_acc -- Depends on a bunch of forked computations
        prnt$ "Set results all available! ("++show size++")"

        -- When debug:
        -- forM_ [0..size] $ \ s -> do 
        --   waitForSetSize s l_acc
        --   prnt$ " ! "++show s++" elements are there in the set.."

        s <- consumeSet l_acc :: Par (Set.Set (Float,Float))
        liftIO (do evaluate s; return ()) -- this explodes
        prnt $ "Done consume set ... "
        prnt $ "Set size: "++show (Set.size s) ++"\n "
        prnt $ "Set sum: "++show (Set.fold (\(_,x) y -> x+y) 0 s)

-- myMapM = parMapM; myMapM_ = parMapM
myMapM = mapM; myMapM_ = mapM_

myfork = fork
-- myfork = id
