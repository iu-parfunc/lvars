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

import Debug.Trace (trace)
import Criterion.Main
import Criterion.Config

import qualified Data.HashTable.IO as H

type HTGraph = H.BasicHashTable Int [Int]

-- Create a graph from a flat list of key-value pairs.
mkHTGraph :: [(Int, Int)] -> IO (HTGraph)
mkHTGraph ls = do
  -- Convert a flat list of key-value pairs to one that maps keys to
  -- lists of values.
  let convert :: (Ord a) => [(a, b)] -> [(a, [b])]
      convert ls = 
        (Map.toList . Map.fromListWith (++) . map (\(x,y) -> (x,[y]))) ls
  ht <- H.fromList (convert ls)
  return ht

-- Graph representation
data Node a = Node
    { label    :: a
    , adjacent :: [Node a]
    }
  deriving Eq

data Graph a = Graph [Node a]
  deriving Show
           
-- Constructing a graph
-- (knot-tying trick from http://stackoverflow.com/a/9732857/415518)
mkGraph :: Eq a => [(a, [a])] -> Graph a
mkGraph links = Graph $ map snd nodeLookupList where
  mkNode (lbl, adj) = (lbl, Node lbl $ map lookupNode adj)
  nodeLookupList = map mkNode links
  lookupNode lbl = fromJust $ lookup lbl nodeLookupList
  
-- Neighbors of a node with a given label
nbrs :: Eq a => Graph a -> a -> [Node a]
nbrs (Graph []) _ = []
nbrs (Graph (n:ns)) lbl =
  if lbl == label n
  then adjacent n
  else nbrs (Graph ns) lbl
       
-- Neighbor labels of a node with a given label
nbrLabels :: (Eq a, Ord a) => Graph a -> a -> [a]
nbrLabels (Graph []) _ = []
nbrLabels (Graph (n:ns)) lbl =
  if lbl == label n
  then map label (adjacent n)
  else nbrLabels (Graph ns) lbl
  
-- Printing a graph
instance (Show a) => Show (Node a) where
  show (Node lbl adj) = show lbl ++ 
                        " --> " ++ show (map (show . label) adj) ++ "\n"
  
-- A graph
graphExample :: Graph Char
graphExample =
    mkGraph [('a', ['b', 'c']),
             ('b', ['a', 'd', 'e']),
             ('c', ['a', 'f', 'g']),
             ('d', ['b', 'h']),
             ('e', ['b', 'f', 'h']),
             ('f', ['c', 'e']),
             ('g', ['c']),
             ('h', ['d', 'e'])]

printGraph :: Show a => a -> IO ()
printGraph g =
  putStrLn . filter (`notElem` "'\"") . show $ g
  
{-

From Ryan's intro draft: "In a graph, find the connected component
containing a vertex V, and compute a function F over the labels of all
vertices in that component, returning a set of results."

Nothing about that problem statement says it has to be a breadth-first
search.  Should it?  (Also, this isn't really a "search", just a
breadth-first traversal.)

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

start_traverse :: (Char -> Char) -> IO (Set.Set Char)
start_traverse f =
  runParIO $ do
    let g = graphExample
    let v = 'a'
    l_acc <- newEmptySet
    -- "manually" add the first element to the set.
    putInSet (f v) l_acc
    result <- bf_traverse g l_acc Set.empty (Set.singleton v) f
    consumeSet l_acc

-- Takes a graph, an LVar, a set of "seen" node labels, a set of "new"
-- node labels, and the function f to be applied to each node.
bf_traverse :: forall a b . (Show a, Ord a, NFData a, Ord b) =>
               (Graph a) -> ISet b -> Set.Set a -> Set.Set a -> (a -> b) ->
               Par (Set.Set b)
bf_traverse g l_acc seen_rank new_rank f =
  -- Nothing in the new_rank set means nothing left to traverse.
  if Set.null new_rank
  then return Set.empty
  else trace ("seen_rank: " ++ show seen_rank ++ "\n" ++
              "new_rank: " ++ show new_rank) $ do
    -- Add new_rank stuff to the "seen" list
    let seen_rank' =  Set.union seen_rank new_rank
    -- Add to the next rank, and to the output/accumulator:
    let add :: a -> Par (Set.Set a)
        add n = if Set.member n seen_rank'
                then return Set.empty
                else do fork $ putInSet (f n) l_acc
                        return (Set.singleton n)
    
    new_rank' <- parMapM (parMapM add . (nbrLabels g)) (Set.toList new_rank)
    
    -- Flatten it out, this should be a parallel fold ideally:
    let new_rank'' = Set.unions $ concat new_rank'
    bf_traverse g l_acc seen_rank' new_rank'' f 



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
  