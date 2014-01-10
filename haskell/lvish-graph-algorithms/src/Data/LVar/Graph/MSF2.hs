{-# LANGUAGE DataKinds, NamedFieldPuns, BangPatterns #-}

-- | Minimum spanning forests in LVish

module Data.LVar.Graph.MSF2 where

import           Control.Monad
import           Data.Vector.Unboxed as U hiding ((++)) 
import qualified Data.Vector         as V
import qualified Data.Set            as S
import           Data.Maybe (fromJust)

import           Control.LVish
import           Control.LVish.Internal (unsafeDet)
import qualified Data.LVar.SLMap as SLM
import qualified Data.LVar.IStructure as IS
import           Data.Graph.Adjacency as Adj

import qualified Data.LVar.MaxCounter as MC

import Data.Par.Range (range)
import Data.Par.Splittable (pforEach, pmapReduce_)


--------------------------------------------------------------------------------

data EdgeGraph = EdgeGraph
                 { edges :: (U.Vector (NodeID, NodeID))
                 , numVerts :: Int }


msf_kruskal :: EdgeGraph -> Par QuasiDet s (IS.IStructure s ())
msf_kruskal EdgeGraph{edges,numVerts} = do
  -- reserves0 <- IS.newIStructure numEdges
  reserves0 <- V.generateM numEdges MC.newMaxCounter -- TODO: could parallelize the alloc?
  round0    <- IS.newIStructure numVerts
  -- Everyone starts in their own singleton set:
  pforEach (range 0 numVerts) $ \ ix -> IS.put round0 ix ix
  round1    <- IS.newIStructure numVerts
  loop reserves0 round0 round1
 where
   numEdges = U.length edges
   loop reserves lastRound nextRound = do

     ------------------------------------- Reserve Phase --------------------
     -- First the reserve phase, for each edge we lock the relevant vertices:
     pforEach (range 0 numEdges) $ \ eid -> do
       let (u,_v) = edges ! eid
       uset <- IS.get lastRound u 
       -- Attempt to reserve one endpoint using edge id as priority:
       MC.put (reserves V.! uset) eid

     ------------------------------------- Commit Phase ---------------------
     pforEach (range 0 numEdges) $ \ eid -> do
       let (u,v) = edges ! eid
       uset <- IS.get lastRound u
       winner <- MC.freezeMaxCounter (reserves V.! uset)
       -- If we are the winner, we commit.
       when (winner == eid) $ do
         vset <- IS.get lastRound v
         -- We link together 
         let new = max uset vset
         IS.put nextRound uset new
         IS.put nextRound vset new
       return ()
       
     -- -- After the loop is finished we can freeze reserves:
     -- pforEach (range 0 numVerts) $ \ vid -> do
     --   winner <- MC.freezeMaxCounter (reserves V.! vid)
     --   -- Take the winning edge ID ...
     --   undefined
       
     loop reserves lastRound nextRound

msf3 :: EdgeGraph -> Par QuasiDet s (IS.IStructure s ())
msf3 EdgeGraph{edges,numVerts} = do
  -- The state keeps track of which set each vertex is in.  Sets are represented by
  -- an elected leader ID from within the set, where the leader is always the minimum ID.
  let state0 = V.generate numVerts id
  state1 <- IS.newIStructure numVerts
  output <- IS.newIStructure numEdges
  -- Everyone starts in their own singleton set:
  -- pforEach (range 0 numVerts) $ \ ix -> IS.put state0 ix ix
  forSpeculative (0,numEdges) (state0,state1) reserve (commit output) update
  return output
  where
    numEdges = U.length edges
    
    reserve ix st = do
      return Nothing
      
    commit finalOutput ix st (Just ()) = do
      -- IS.put 
      return ()
      
    update (_,newState) = do
      is2 <- IS.newIStructure numEdges
      -- Freeze the values we wrote in the commit phase.
      vec <- IS.freezeIStructure newState
      let fn ix Nothing  = ix
          fn _ (Just id) = id
      return (V.imap fn vec, is2)

-- | Inefficient, out-of-place deterministic reservations.
--      
--   A barrier separates the reserve, commit, and update phases.
--
--   The update phase enables the creation of "single use" LVars that are frozen in
--   either the reserve or commit rounds and reallocated in the next round.
forSpeculative :: (Ord b) =>
                  (Int,Int)             -- ^ Start and end (inclusive/exclusive)
                -> st                    -- ^ Initial state
                -> (Int -> st -> Par d s (Maybe b)) -- ^ Reserve function
                -> (Int -> st -> b -> Par d s ())   -- ^ Commit function
                -> (st -> Par d s st)    -- ^ State update function after each round of commits.
                -> Par d s ()
forSpeculative (st,en) state0 reserve commit update = do 
  hp <- newPool
  mainloop hp 0 state0 S.empty 0 
 where
   mainloop hp !round state leftover numberDone
    | numberDone == total = return ()
    | otherwise = do
     let remain     = total - numberDone
         prefixSize = (min maxRound remain)
         prefixEnd  = numberDone + prefixSize
     logDbgLn 3 $ " [dbg-lvish] forSpeculative starting round "++
                  show round++": numDone "++show numberDone++", of "++show total

     reserved <- IS.newIStructure prefixSize

     -- TODO: fork a task for the leftovers...     
{-
     -- Would be nice to have a parallel generate for a BitVector here:
     winners <- pmapReduce_ (range numberDone prefixEnd)
       (\ ix -> do b <- reserve ix state
                   case b of
                     Nothing  -> return S.empty
                     Just res -> return $! S.singleton (ix,res))
       (\ a b -> return $! S.union a b)
       S.empty
     -- FIXME: need good splittable parallel map on sets:
-}
     pforEach (range numberDone prefixEnd) $ \ ix -> do 
        b <- reserve ix state
        case b of 
          Nothing  -> return () -- FIXME: Failed iterates need to be collected for later.
          Just res -> IS.put_ reserved ix res
     reserved' <- unsafeDet $ IS.freezeIStructure reserved
--     quiesce hp
     logDbgLn 3 $ " [dbg-lvish] forSpeculative finished reserve phase, round "++show round
     
     pforEach (range numberDone prefixEnd) $ \ ix -> do
       case reserved' V.! ix of
         Nothing  -> return ()
         Just val -> commit ix state val

--     quiesce hp
     logDbgLn 3 $ " [dbg-lvish] forSpeculative finished commit phase, round "++show round
     state' <- update state
     logDbgLn 3 $ " [dbg-lvish] forSpeculative finished update phase, round "++show round     

     let leftover'   = undefined
         numberDone' = undefined
     mainloop hp (round+1) state' leftover' numberDone'
     error "FINSHME"
   
   total = en - st
   maxRound = max 1 (total `quot` granularity)
   granularity = 20

