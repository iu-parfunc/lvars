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
import Data.List.Split
import System.IO

-- So we can see the behavior of bf_traverse
import Debug.Trace (trace)

-- For benchmarking
import Criterion.Main
import Criterion.Config

-- For representing graphs
-- import qualified Data.HashTable.IO as H
import qualified Data.Vector as V

-- a Graph is an IOHashTable Basic Int [Int]
-- type Graph = H.BasicHashTable Int [Int]
type Graph = V.Vector [Int]

-- Create a graph from a flat list of key-value pairs.
mkGraph :: [(Int, Int)] -> IO Graph
mkGraph ls = do
  -- Convert a flat list of key-value pairs to one that maps keys to
  -- lists of values.
  let convert :: (Ord a) => [(a, b)] -> [(a, [b])]
      convert = 
        Map.toList . Map.fromListWith (++) . map (\(x,y) -> (x,[y]))
  return (V.fromList $ map snd $ convert ls)

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
  let (ignored:stringpairs) = map tuplify2 (map (splitOn " ") (lines inStr))
  mkGraph (map (\(x,y) -> (read x::Int, read y::Int)) stringpairs)
  
-- Neighbors of a node with a given label
nbrs :: Graph -> Int -> IO [Int]
nbrs g lbl = do
--  maybeVals <- H.lookup g lbl
  -- answer <- case maybeVals of
  --   Just vals -> return vals
  --   Nothing -> return []
  -- return answer
  return (g V.! lbl)

-- A tiny example graph
graphExample :: IO Graph
graphExample = do  
  let g = [(0,[]), -- RRN, added this node because vectors are zero-based
           (1, [2, 3]),
           (2, [1, 4, 5]),
           (3, [1, 6, 7]),
           (4, [2, 8]),
           (5, [2, 6, 8]),
           (6, [3, 5]),
           (7, [3]),
           (8, [4, 5])]
  return (V.fromList $ map snd $ g) -- No need for IO actually.
--  ht <- H.new
--  mapM (\x -> H.insert ht (fst x) (snd x)) g 
--  return ht

printGraph :: Graph -> IO ()
printGraph g = do
  let ls = V.toList g
  putStrLn (show ls)
  return ()

{-
-- Just experimenting here.
foo = do
  g1 <- mkGraphFromFile
  printGraph g1
  g2 <- graphExample
  printGraph g2
  nbrs g2 1
-}

main = defaultMainWith defaultConfig (return ()) [
         bgroup "bf_traverse" [
           bench "bf_traverse with identity" $ start_traverse (\x -> x)
         -- , bench "bf_traverse with identity" $ start_traverse (\x -> x)
         -- , bench "bf_traverse with identity" $ start_traverse (\x -> x)
         ]--,
         -- bgroup "bf_traverse" [
         --   bench "bf_traverse with identity" $ start_traverse (\x -> x)
         -- , bench "bf_traverse with identity" $ start_traverse (\x -> x)
         -- , bench "bf_traverse with identity" $ start_traverse (\x -> x)
         -- ]
       ]

start_traverse :: (Int -> Int) -> IO (Set.Set Int)
start_traverse f = do
      g <- graphExample
      let v = 1
      runParIO $ do
        l_acc <- newEmptySet
        -- "manually" add the first element to the set.
        putInSet (f v) l_acc
        result <- bf_traverse g l_acc Set.empty (Set.singleton v) f
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
    -- Grab all the neighbor nodes of everything in the new_rank set.
    -- That is, map (nbrs g) over (Set.toList new_rank).
    
    -- e.g., for graphExample, if new_rank was [1, 2, 3],
    -- new_rank_nbrs should be [[2,3], [1,4,5], [1,6,7]]
                        
    -- Then, take each of those lists and map 'add' over them.
    
    -- new_rank_nbrs <- parMapM (nbrs g) (Set.toList new_rank)
    -- new_rank' <- parMapM add new_rank_nbrs
    
    -- Flatten it out, this should be a parallel fold ideally:
    -- let new_rank'' = Set.unions $ concat new_rank'
    bf_traverse g l_acc seen_rank new_rank f -- should really be new_rank''
