import LVarTraceInternal
import Data.Maybe (fromJust)
import qualified Data.Set as Set

--------------------------------------------------------------------------------

-- | OK, so I want to do a breadth-first search of a graph...I need a
-- graph to search, then!

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
  
main =
  printGraph graphExample
