{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE CPP #-}
#ifdef PURE
#warning "Using the PURE version"
import LVarTracePure (newEmptySet, putInSet, Par, runParIO, ISet, consumeSet, fork)
#else
import LVarTraceInternal (newEmptySet, putInSet, Par, runParIO, ISet, consumeSet, fork)
#endif

import Control.Monad.Par.Combinator (parMapM)
import Control.Monad.Par.Class (ParFuture)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Control.DeepSeq (NFData)
import Data.Traversable (Traversable)
import Data.Map as Map (toList, fromListWith)

-- For parsing the file produced by pbbs
import System.IO
import Data.List.Split

-- So we can see the behavior of bf_traverse
import Debug.Trace (trace)

-- For benchmarking
import Criterion.Main
import Criterion.Config

import qualified Data.HashTable.IO as H

-- a Graph is an IOHashTable Basic Int [Int]
type Graph = H.BasicHashTable Int [Int]

-- Create a graph from a flat list of key-value pairs.
mkGraph :: [(Int, Int)] -> IO Graph
mkGraph ls = do
  -- Convert a flat list of key-value pairs to one that maps keys to
  -- lists of values.
  let convert :: (Ord a) => [(a, b)] -> [(a, [b])]
      convert = 
        Map.toList . Map.fromListWith (++) . map (\(x,y) -> (x,[y]))
  ht <- H.fromList (convert ls)
  return ht

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
nbrs :: Graph -> Int -> IO ([Int])
nbrs g lbl = do
  maybeVals <- H.lookup g lbl
  answer <- case maybeVals of
    Just vals -> return vals
    Nothing -> return []
  return answer
  
-- A tiny example graph
graphExample :: IO Graph
graphExample = do
  ht <- H.new
  mapM (\x -> H.insert ht (fst x) (snd x)) [(1, [2, 3]),
                                            (2, [1, 4, 5]),
                                            (3, [1, 6, 7]),
                                            (4, [2, 8]),
                                            (5, [2, 6, 8]),
                                            (6, [3, 5]),
                                            (7, [3]),
                                            (8, [4, 5])]
  return ht

printGraph :: Graph -> IO ()
printGraph g = do
  ls <- H.toList g
  putStrLn (show ls)
  -- putStrLn . filter (`notElem` "'\"") (show ls)
  return ()
  
main = do
  g1 <- mkGraphFromFile
  printGraph g1
  g2 <- graphExample
  printGraph g2

{-

From Ryan's intro draft: "In a graph, find the connected component
containing a vertex V, and compute a function F over the labels of all
vertices in that component, returning a set of results."

Nothing about that problem statement says it has to be a breadth-first
search.  Should it?  (Also, this isn't really a "search", just a
breadth-first traversal.)

-}

-- main = defaultMainWith defaultConfig (return ()) [
--          bgroup "bf_traverse" [
--            bench "bf_traverse with identity" $ start_traverse (\x -> x)
--          -- , bench "bf_traverse with identity" $ start_traverse (\x -> x)
--          -- , bench "bf_traverse with identity" $ start_traverse (\x -> x)
--          ]--,
--          -- bgroup "bf_traverse" [
--          --   bench "bf_traverse with identity" $ start_traverse (\x -> x)
--          -- , bench "bf_traverse with identity" $ start_traverse (\x -> x)
--          -- , bench "bf_traverse with identity" $ start_traverse (\x -> x)
--          -- ]
--        ]

-- start_traverse :: (Int -> Int) -> IO (Set.Set Int)
-- start_traverse f =
--   runParIO $ do
--     let g = do 
--           graph <- mkGraphFromFile
--           return graph
--     let v = 1
--     l_acc <- newEmptySet
--     -- "manually" add the first element to the set.
--     putInSet (f v) l_acc
--     result <- bf_traverse g l_acc Set.empty (Set.singleton v) f
--     consumeSet l_acc

-- Takes a graph, an LVar, a set of "seen" node labels, a set of "new"
-- node labels, and the function f to be applied to each node.
-- bf_traverse :: forall a . (Ord a) =>
--                Graph -> ISet a -> Set.Set Int -> Set.Set Int -> (Int -> a) ->
--                Par (Set.Set a)
-- bf_traverse g l_acc seen_rank new_rank f =
--   -- Nothing in the new_rank set means nothing left to traverse.
--   if Set.null new_rank
--   then return Set.empty
--   else trace ("seen_rank: " ++ show seen_rank ++ "\n" ++
--               "new_rank: " ++ show new_rank) $ do
--     -- Add new_rank stuff to the "seen" list
--     let seen_rank' =  Set.union seen_rank new_rank
--     -- Add to the next rank, and to the output/accumulator:
--     let add :: a -> Par (Set.Set a)
--         add n = if Set.member n seen_rank'
--                 then return Set.empty
--                 else do fork $ putInSet (f n) l_acc
--                         return (Set.singleton n)
    
--     new_rank' <- parMapM (parMapM add . (nbrs g)) (Set.toList new_rank)
    
--     -- Flatten it out, this should be a parallel fold ideally:
--     let new_rank'' = Set.unions $ concat new_rank'
--     bf_traverse g l_acc seen_rank' new_rank'' f 

--------------------------------------------------------------------------------
-- Oops, turns out there is a painful reason that we don't have a traversable
-- instance for Set:
--
-- http://www.haskell.org/pipermail/haskell-cafe/2010-July/080978.html
--------------------------------------------------------------------------------
    
-- instance Functor Set.Set where
--   fmap f s = Set.map f s
--   -- fromList $ fmap f $ Set.toList s  

-- instance Traversable Set.Set where
  