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
-- Generate /tmp/grid using pbbs:
-- ./gridGraph -d 3 10000 /tmp/grid
-- or something along those lines.
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

-- A tiny example graph
graphExample :: Graph
graphExample = 
  let g :: [(Int,[Int])]
      g = [(0, [1]),
           (1, [2, 3]),
           (2, [1, 4, 5]),
           (3, [1, 6, 7]),
           (4, [2, 8]),
           (5, [2, 6, 8]),
           (6, [3, 5]),
           (7, [3]),
           (8, [4, 5])]
  in (V.fromList $ map snd $ g)

printGraph :: Graph -> IO ()
printGraph g = do
  let ls = V.toList g
  putStrLn (show ls)
  return ()

main :: IO ()
main = do
  g <- mkGraphFromFile
  let startNode = 0
  let graphThunk = start_traverse g 0
  let f = (\x -> x) -- todo: more interesting function?
  defaultMainWith defaultConfig (return ()) [
         bgroup "bf_traverse" [
           bench "bf_traverse with identity" $ graphThunk f
         ]
         ]

-- Takes a graph, a start node, and a function to be applied to each
-- node.
start_traverse :: Graph -> Int -> (Int -> Int) -> IO (Set.Set Int)
start_traverse g startNode f = do
      runParIO $ do
        l_acc <- newEmptySet
        -- "manually" add startNode
        putInSet (f startNode) l_acc
        result <- bf_traverse g l_acc Set.empty (Set.singleton startNode) f
        consumeSet l_acc

-- Takes a graph, an LVar, a set of "seen" node labels, a set of "new"
-- node labels, and the function f to be applied to each node.
bf_traverse :: (NFData (Set.Set Int),
                Show (Set.Set Int), 
                Ord (Set.Set Int))
                => 
               Graph -> ISet Int -> Set.Set Int -> Set.Set Int -> (Int -> Int)
               -> Par (Set.Set Int)
bf_traverse g l_acc seen_rank new_rank f =
  -- Nothing in the new_rank set means nothing left to traverse.
  if Set.null new_rank
  then return Set.empty
  else trace ("seen_rank: " ++ show seen_rank ++ "\n" ++
              "new_rank: " ++ show new_rank) $ do
    -- Add new_rank stuff to the "seen" list
    let seen_rank' =  Set.union seen_rank new_rank
    -- Add to the next rank, and to the output/accumulator:
    let add :: Int -> Par (Set.Set Int)
        add n = if Set.member n seen_rank'
                then return Set.empty
                else do fork $ putInSet (f n) l_acc
                        return (Set.singleton n)
    -- Grab the neighbor nodes of everything in the new_rank set.
    let parMapMAdd = parMapM add :: [Int] -> Par [Set.Set Int]
    let getNeighbors = parMapMAdd . (nbrs g) :: Int -> Par [Set.Set Int]
    new_rank' <- parMapM getNeighbors (Set.toList new_rank)
    
    -- Flatten it out, this should be a parallel fold ideally:
    let new_rank'' = Set.unions $ concat new_rank'
    bf_traverse g l_acc seen_rank' new_rank'' f
