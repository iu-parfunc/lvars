{-# LANGUAGE FlexibleContexts #-}

import LVarTraceInternal (newEmptySet, putInSet, Par, runParIO, ISet)
import Control.Monad.Par.Combinator (parMap)
import Control.Monad.Par.Class (ParFuture)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Control.DeepSeq (NFData)
import Data.Traversable (Traversable)

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
       
-- Neighbor labels of a node with a given label, as a set
nbrLabels :: (Eq a, Ord a) => Graph a -> a -> Set.Set a
nbrLabels (Graph []) _ = Set.empty
nbrLabels (Graph (n:ns)) lbl =
  if lbl == label n
  then Set.fromList (map label (adjacent n))
  else nbrLabels (Graph ns) lbl
  
-- Printing a graph
instance (Show a) => Show (Node a) where
  show (Node lbl adj) = show lbl ++ 
                        " --> " ++ show (map (show . label) adj) ++ "\n"
  
-- A graph
graphExample =
    mkGraph [('a', ['b', 'c']),
             ('b', ['a', 'd', 'e']),
             ('c', ['a', 'f', 'g']),
             ('d', ['b', 'h']),
             ('e', ['b', 'f', 'h']),
             ('f', ['c', 'e']),
             ('g', ['c']),
             ('h', ['d', 'e'])]

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

main =
  runParIO $ do
    let g = graphExample
    let v = 'a'
    let f = \x -> x
    l_acc <- newEmptySet
    result <- bf_traverse g l_acc Set.empty (Set.singleton v) f
    return result

-- Takes a graph, an LVar, a set of "seen" node labels, a set of "new"
-- node labels, and the function f to be applied to each node.  We're
-- not actually doing anything with f yet.
bf_traverse :: (Graph a) -> ISet a ->
                  Set.Set a -> Set.Set a -> (a -> b) -> Par (Set.Set b)
bf_traverse g l_acc seen_rank new_rank f =
  if Set.null new_rank
  then return Set.empty
  else do
    let seen_rank' = Set.union seen_rank new_rank
    -- Add to the next rank, and to the output/accumulator:
    let add n = if Set.member n seen_rank'
                then return Set.empty
                else do putInSet n l_acc
                        return (Set.singleton n)
    let new_rank' =
          foldr Set.union Set.empty (parMap (parMap add . (nbrLabels g))
                                     new_rank)
    bf_traverse g l_acc seen_rank' new_rank' f

