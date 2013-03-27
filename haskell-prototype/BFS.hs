{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

#ifdef PURE
#warning "Using the PURE version"
import LVarTracePure (newEmptySet, putInSet, Par, runParIO, ISet, consumeSet, fork, liftIO)
#else
import LVarTraceInternal (newEmptySet, putInSet, Par, runParIO, ISet, consumeSet, fork, liftIO)
#endif

import GHC.Conc (numCapabilities)
import Control.DeepSeq (deepseq)
import Control.Exception (evaluate)
import Control.Monad.Par.Combinator (parMapM)
import Control.Monad.Par.Class (ParFuture)
import qualified Data.Set as Set
import Control.DeepSeq (NFData)
import Data.Traversable (Traversable)
import Data.Map as Map (toList, fromListWith)

-- For parsing the file produced by pbbs
import Data.List.Split (splitOn)
import System.IO (openFile, hGetContents, IOMode(ReadMode))

-- So we can see the behavior of bf_traverse
import Debug.Trace (trace)

-- For benchmarking
import Criterion.Main
import Criterion.Config

-- For representing graphs
import qualified Data.Vector as V

-- Vector representation of graphs: the list at index k is the list of
-- node k's neighbors.
type Graph = V.Vector [Int]

-- Create a graph from a flat list of key-value pairs.
mkGraph :: [(Int, Int)] -> Graph
mkGraph ls = 
  -- Convert a flat list of key-value pairs to a Graph.
  let convert :: (Ord a) => [(a, b)] -> [(a, [b])]
      convert = 
        Map.toList . Map.fromListWith (++) . map (\(x,y) -> (x,[y]))
  in (V.fromList $ map snd $ convert ls)

-- Slurp in key-value pairs from a file in pbbs EdgeArray format.
mkGraphFromFile :: IO Graph
mkGraphFromFile = do  
  inh <- openFile "/tmp/grid" ReadMode
  inStr <- hGetContents inh
  let tuplify2 [x,y] = (x, y)
  -- Ignore the initial "EdgeArray" string in the pbbs-generated file.
  let (_:stringpairs) = map tuplify2 (map (splitOn " ") (lines inStr))
  -- return (mkGraph (map (\(x,y) -> (read x::Int, read y::Int)) stringpairs))
  g <- evaluate (mkGraph (map (\(x,y) -> (read x::Int, read y::Int)) stringpairs))
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

myConfig :: Config
myConfig = defaultConfig {
  cfgSamples = ljust 10,
  cfgPerformGC = ljust True
}

prnt :: String -> Par ()
prnt str = trace str $ return ()

main :: IO ()
main = do
  g <- mkGraphFromFile
  let startNode = 0
  
  let graphThunk :: (Float -> Float) -> IO ()
      graphThunk fn = do st <- start_traverse g 0 fn
                         putStrLn "Done with start_traverse, going to pop the final thunk."
                         evaluate st
                         putStrLn "Done!"
--  let sin_iter_count = sin_iter 10000000
  let sin_iter_count = sin_iter 10

#ifdef NOCRITERION
  graphThunk sin_iter_count
#else
  defaultMainWith myConfig (return ()) [
         bgroup "bf_traverse" [
           bench "sin_iter" $ graphThunk sin_iter_count
         ]
         ]
#endif

-- Takes a graph, a start node, and a function to be applied to each
-- node.
start_traverse :: Graph -> Int -> (Float -> Float) -> IO (Set.Set Float)
start_traverse !g startNode f = do
  runParIO $ do
        prnt $ "Running on " ++ show numCapabilities ++ " parallel resources..."
        l_acc <- newEmptySet
        -- "manually" add startNode
        putInSet (f (fromIntegral startNode)) l_acc
        result <- bf_traverse 0 g l_acc Set.empty (Set.singleton startNode) f
        prnt $ "Evaling result..."
        liftIO (do evaluate result; return ())
        prnt $ "Done with bf_traverse... "
        s <- consumeSet l_acc :: Par (Set.Set Float)
        liftIO (do evaluate s; return ()) -- this explodes
        prnt $ "Done consume set ... "
        return s

-- Takes a graph, an LVar, a set of "seen" node labels, a set of "new"
-- node labels, and the function f to be applied to each node.
bf_traverse :: Int -> Graph -> ISet Float -> Set.Set Int -> Set.Set Int -> (Float -> Float)
               -> Par (Set.Set Float)
bf_traverse n !g !l_acc !seen_rank !new_rank !f = do 
  trace ("bf_traverse call.. "++show n++" seen/new size "
         ++show (Set.size seen_rank, Set.size new_rank)) $ return () 
  
  -- Nothing in the new_rank set means nothing left to traverse.
  if Set.null new_rank
  then return Set.empty
  else do
  -- else trace ("seen_rank: " ++ show seen_rank ++ "\n" ++
  --             "new_rank: " ++ show new_rank) $ do
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
    bf_traverse (n+1) g l_acc seen_rank' new_rank'' f

-- myMapM = parMapM
myMapM = mapM

