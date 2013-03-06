import LVarTraceInternal
import Data.Maybe (fromJust)

--------------------------------------------------------------------------------

-- | OK, so I want to do a breadth-first search of a graph...I need a
-- graph to search, then!

-- Graph representation
data Node a = Node
    { label    :: a
    , adjacent :: [Node a]
    }

data Graph a = Graph [Node a]
  deriving Show
           
-- Constructing a graph
-- (knot-tying trick from http://stackoverflow.com/a/9732857/415518)
mkGraph :: Eq a => [(a, [a])] -> Graph a
mkGraph links = Graph $ map snd nodeLookupList where
  mkNode (lbl, adj) = (lbl, Node lbl $ map lookupNode adj)
  nodeLookupList = map mkNode links
  lookupNode lbl = fromJust $ lookup lbl nodeLookupList
  
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
