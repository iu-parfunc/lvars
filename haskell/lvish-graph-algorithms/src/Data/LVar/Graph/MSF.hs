-- | Minimum spanning forests in LVish

module Data.LVar.Graph.MSF where

import Control.Monad
import Control.LVish
import qualified Data.LVar.SLMap as SLM
import qualified Data.LVar.IStructure as IS
import Data.Graph.Adjacency as Adj
import qualified Data.Vector.Unboxed as U

--- A NodeInfo is a 2-element array. First element is parentID, second element is rank
--- Taking advantage of the fact that: type of parentID = Adj.NodeID = Int = type of rank

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


-- Implements "union by rank", where the rank approximates subtree size

union :: DisjointSet s -> NodeID -> NodeID -> Par d s ()
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
  return ()
                 

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


-- Using EdgeGraph representation for this algorithm. Defining it here for now.

data EdgeGraph = EdgeGraph (U.Vector (NodeID, NodeID))

type ParFor d s = (Int,Int) -> (Int -> Par d s ()) -> Par d s ()

-- Minimum spanning forest via Kruskal's algorithm

msf_kruskal :: ParFor d s -> EdgeGraph -> Par d s (IS.IStructure s Bool)
msf_kruskal parFor (EdgeGraph edges) = do
  msf <- IS.newIStructure (U.length edges)
  let maxV = U.foldl (\a (u,v) -> max a $ max u v) 0 edges
      vids = [0..maxV]
  infos <- mapM make_set vids
  ds <- SLM.newFromList $ zip vids infos
  parFor (0,U.length edges) $ \ ed -> do
    let (u,v) = edges U.! (fromIntegral ed)
    [uset, vset] <- mapM (find_set ds) [u,v]
    when (uset /= vset) $ IS.put_ msf (fromIntegral ed) True >> union ds u v
  return msf