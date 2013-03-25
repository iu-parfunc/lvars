{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE CPP #-}
#ifdef PURE
#warning "Using the PURE version"
import LVarTracePure (newEmptySet, putInSet, Par, runParIO, ISet, consumeSet, fork)
#else
import LVarTraceInternal (newEmptySet, putInSet, Par, runParIO, ISet, consumeSet, fork)
#endif

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
  return (mkGraph (map (\(x,y) -> (read x::Int, read y::Int)) stringpairs))
  
-- Neighbors of a node with a given label
nbrs :: Graph -> Int -> [Int]
nbrs g lbl = g V.! lbl

printGraph :: Graph -> IO ()
printGraph g = do
  let ls = V.toList g
  putStrLn (show ls)
  return ()
    
-- Iterates the sin function x times on its input.  This takes a few
-- seconds for, e.g., n = 10,000,000.
sin_iter :: (Floating a) => Int -> a -> a
sin_iter 0 x = x
sin_iter n x = sin_iter (n - 1) (sin x)

myConfig = defaultConfig {
  cfgSamples = ljust 10,
  cfgPerformGC = ljust True
}

main :: IO ()
main = do
  g <- mkGraphFromFile
  let startNode = 0
      
  -- We have to pick a particular Floating type for the result of
  -- sin_iter, or the typechecker will get confused.
  let graphThunk = start_traverse g 0 :: (Float -> Float) -> IO (Set.Set Float)
  let sin_iter_ten_million = sin_iter 10000000
  
  defaultMainWith myConfig (return ()) [
         bgroup "bf_traverse" [
           bench "sin_iter_ten_million" $ graphThunk sin_iter_ten_million
         ]
         ]

-- Takes a graph, a start node, and a function to be applied to each
-- node.
start_traverse :: (NFData a,
                   Show a,
                   Floating a,
                   Ord a)
                  =>
                  Graph -> Int -> (a -> a) -> IO (Set.Set a)
start_traverse g startNode f = do
      runParIO $ do
        l_acc <- newEmptySet
        -- "manually" add startNode
        putInSet (f (fromIntegral startNode)) l_acc
        result <- bf_traverse g l_acc Set.empty (Set.singleton startNode) f
        consumeSet l_acc

-- Takes a graph, an LVar, a set of "seen" node labels, a set of "new"
-- node labels, and the function f to be applied to each node.
bf_traverse :: (NFData (Set.Set a),
                Show (Set.Set a), 
                Ord (Set.Set a),
                Ord a,
                Floating a)
               => 
               Graph -> ISet a -> Set.Set Int -> Set.Set Int -> (a -> a)
               -> Par (Set.Set a)
bf_traverse g l_acc seen_rank new_rank f =
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
    let parMapMAdd = parMapM add :: [Int] -> Par [Set.Set Int]
    let getNeighbors = parMapMAdd . (nbrs g) :: Int -> Par [Set.Set Int]
    new_rank' <- parMapM getNeighbors (Set.toList new_rank)
    
    -- Flatten it out, this should be a parallel fold ideally:
    let new_rank'' = Set.unions $ concat new_rank'
    bf_traverse g l_acc seen_rank' new_rank'' f
