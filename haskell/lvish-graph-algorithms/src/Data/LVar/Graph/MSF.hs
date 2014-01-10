-- | Use Data.LVar.SLMap to represent disjoint-set data structure
-- Keys are node IDs
-- Values are a record containing nodeID, parentID, rank of parent
-- Need to make record an instance of Eq, based on nodeID

-- | Minimum spanning forests in LVish

module Data.LVar.Graph.MSF where

import Control.Monad
import Control.LVish
import qualified Data.LVar.SLMap as SLM
import qualified Data.LVar.IStructure as IS
import Data.Graph.Adjacency as Adj

-- Having the id inside NodeInfo is a hack to use inserts as "updates"
-- and to circumvent any multiple-puts errors

-- data NodeInfo = NI {id :: NodeID, parent :: NodeID, rank :: Int}

-- Defining this instance on the assumption that SLMap inserts on pre-existing
-- keys will raise a multiple put error unless the values are the same (==)

-- ASSUMPTION: inserting on an existing key with an == value will put this
-- "new" value against the key

-- instance Eq NodeInfo where
--     NI a _ _ == NI b _ _ = a == b
--     NI a _ _ /= NI b _ _ = a /= b

-- A NodeInfo is a 2-element array. First element is parentID, second element is rank
-- Taking advantage of the fact that: type of parentID = Adj.NodeID = Int = type of rank
type NodeInfo s = IS.IStructure s Int

parent :: NodeInfo s -> Par d s NodeID
parent info = IS.get info 0

rank :: NodeInfo s -> Par d s Int
rank info = IS.get info 1

type DisjointSet s = SLM.IMap Int s (NodeInfo s)

make_set :: NodeID -> Par d s (NodeInfo s)
make_set nd = do 
  ni <- IS.newIStructure 2
  IS.put ni 0 nd >> return ni

union :: DisjointSet s -> NodeID -> NodeID -> Par d s (DisjointSet s)
union ds x y = do
  [px,py] <- mapM (find_set ds) [x,y]
  rankPx <- SLM.getKey px ds >>= rank
  rankPy <- SLM.getKey py ds >>= rank
  if rankPx > rankPy 
    then SLM.modify ds py (make_set py) $ update_parent px
    else SLM.modify ds px (make_set px) $ update_parent py
  when (rankPx == rankPy) $ do
       SLM.modify ds py (make_set py) $ update_rank (rankPy + 1)
       return ()
  return ds
                 

-- Implements path compression

find_set :: DisjointSet s -> NodeID -> Par d s NodeID
find_set ds nd = do
  info <- SLM.getKey nd ds
  p <- parent info
  when (nd /= p) $ do
    new_p <- find_set ds p
    SLM.modify ds nd (make_set nd) $ update_parent new_p
    return ()
  SLM.getKey nd ds >>= parent


-- Both types of updates return a new NodeInfo since multiple puts are not allowed

update_parent :: NodeID -> NodeInfo s -> Par d s (NodeInfo s)
update_parent pID info = do
  ni <- IS.newIStructure 2
  IS.put ni 0 pID >> rank info >>= IS.put ni 1 >> return ni

update_rank :: Int -> NodeInfo s -> Par d s (NodeInfo s)
update_rank r info = do
  ni <- IS.newIStructure 2
  parent info >>= IS.put ni 0 >> IS.put ni 1 r >> return ni
  
