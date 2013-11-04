-- | Maximum independent set algorithms in lvish

{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Data.LVar.Graph.MIS where

import Data.Set as Set

import Utils

-- Benchmark utils:
import PBBS.FileReader
import PBBS.Timing (wait_clocks, runAndReport)
-- calibrate, measureFreq, commaint,

import Control.LVish
import Control.LVish.Internal
import Control.LVish.DeepFrz (runParThenFreezeIO)
import qualified Control.LVish.SchedIdempotent as L

import Control.Monad
import Control.Monad.Par.Combinator (parFor, InclusiveRange(..))
import Control.Monad.ST
import Control.Exception
import GHC.Conc

import Data.Word
import Data.Maybe
import Data.LVar.MaxCounter as C
import Data.Time.Clock
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Storable as UV
import qualified Data.Vector.Storable.Mutable as MV
import System.Mem (performGC)
import System.Environment (getArgs)
import System.Directory
import System.Process


-- define DEBUG_CHECKS

--------------------------------------------------------------------------------

#if 1
import Data.LVar.PureSet as S
#else
-- [2013.07.09] This one still isn't terminating on 125K+
--  Well, maybe it's just slow... 5000 takes 2 seconds.
--  Yes, it's literally over 100 times slower currently.
import Data.LVar.SLSet as S
#endif

import qualified Data.LVar.SLSet as SL

import Data.LVar.IStructure as ISt
import Data.LVar.NatArray as NArr

------------------------------------------------------------------------------------------
-- Maximal Independent Set
------------------------------------------------------------------------------------------  

-- Lattice where undecided = bot, and chosen/nbrchosen are disjoint middle states
flag_UNDECIDED :: Word8
flag_CHOSEN    :: Word8
flag_NBRCHOSEN :: Word8
flag_UNDECIDED = 0
flag_CHOSEN    = 1
flag_NBRCHOSEN = 2

{-# INLINE maximalIndependentSet #-}
-- maximalIndependentSet :: ISet s NodeID -> Par d s (ISet s NodeID)  -- Operate on a subgraph
-- maximalIndependentSet :: AdjacencyGraph -> Par d s (ISet s NodeID) -- Operate on a whole graph.
maximalIndependentSet :: ParFor d s -> AdjacencyGraph -> Par d s (NatArray s Word8) -- Operate on a whole graph.
maximalIndependentSet parFor gr@(AdjacencyGraph vvec evec) = do
  -- For each vertex, we record whether it is CHOSEN, not chosen, or undecided:
  let numVerts = U.length vvec
  flagsArr :: NatArray s Word8 <- newNatArray numVerts
  let       
      -- Here's the loop that scans through the neighbors of a node.
      loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          -- logStrLn$ " [MIS]   ... on nbr "++ show i++" of "++show numNbrs
          let nbrInd = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          -- If we got to the end of the neighbors below us, then we are NOT disqualified:
          if nbrInd > selfInd
            then thisNodeWins
            else do
              -- This should never block in a single-thread execution:
              logStrLn (" [MIS] ! Getting on nbrInd "++show nbrInd)
              nbrFlag <- NArr.get flagsArr (fromIntegral nbrInd)
              logStrLn (" [MIS] ! Get completed on nbrInd "++show nbrInd)
              if nbrFlag == flag_CHOSEN
                then NArr.put flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = logStrLn (" [MIS] ! Node chosen: "++show selfInd) >> 
                         NArr.put flagsArr (fromIntegral selfInd) flag_CHOSEN
  parFor (0,numVerts) $ \ ndIx -> do 
      let nds = nbrs gr (fromIntegral ndIx)
      -- logStrLn$ " [MIS] processing node "++show ndIx++" nbrs "++show nds
      loop (U.length nds) nds ndIx  0
  return flagsArr

-- | DUPLICATE CODE: IStructure version.
maximalIndependentSet2 :: ParFor d s -> AdjacencyGraph -> Par d s (IStructure s Word8) -- Operate on a whole graph.
maximalIndependentSet2 parFor gr@(AdjacencyGraph vvec evec) = do
  logStrLn$ " [MIS] Beginning maximalIndependentSet / Istructures"
  -- For each vertex, we record whether it is CHOSEN, not chosen, or undecided:
  let numVerts = U.length vvec
  flagsArr <- newIStructure numVerts
  let       
      -- Here's the loop that scans through the neighbors of a node.
      loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          -- logStrLn$ " [MIS]   ... on nbr "++ show i++" of "++show numNbrs
          let nbrInd = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          -- If we got to the end of the neighbors below us, then we are NOT disqualified:
          if nbrInd > selfInd
            then thisNodeWins
            else do
              -- This should never block in a single-thread execution:
              logStrLn (" [MIS] ! Getting on nbrInd "++show nbrInd)
              nbrFlag <- ISt.get flagsArr (fromIntegral nbrInd)
              logStrLn (" [MIS] ! Get completed on nbrInd "++show nbrInd)
              if nbrFlag == flag_CHOSEN
                then ISt.put_ flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = logStrLn (" [MIS] ! Node chosen: "++show selfInd) >> 
                         ISt.put_ flagsArr (fromIntegral selfInd) flag_CHOSEN
  parFor (0,numVerts) $ \ ndIx -> do 
      let nds = nbrs gr (fromIntegral ndIx)
      -- logStrLn$ " [MIS] processing node "++show ndIx++" nbrs "++show nds
      loop (U.length nds) nds ndIx  0
  return flagsArr


-- | Sequential version.
maximalIndependentSet3 :: AdjacencyGraph -> (U.Vector Word8)
maximalIndependentSet3 gr@(AdjacencyGraph vvec evec) = U.create $ do
  let numVerts = U.length vvec
  flagsArr <- M.replicate numVerts 0
  let loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          let nbrInd   = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          if nbrInd > selfInd
            then thisNodeWins
            else do
              nbrFlag <- M.read flagsArr (fromIntegral nbrInd)
              if nbrFlag == flag_CHOSEN
                then M.write flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = M.write flagsArr (fromIntegral selfInd) flag_CHOSEN
  for_ (0,numVerts) $ \ ndIx -> do 
      let nds = nbrs gr (fromIntegral ndIx)
      loop (U.length nds) nds ndIx 0
  return flagsArr

-- | Sequential version on NatArray...
maximalIndependentSet3B :: AdjacencyGraph -> (UV.Vector Word8) -> (UV.Vector Word8)
maximalIndependentSet3B gr@(AdjacencyGraph vvec evec) vec = UV.create $ do
  let numVerts = U.length vvec
  flagsArr <- MV.replicate numVerts 0
  let loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          let nbrInd   = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          if nbrInd > selfInd
            then thisNodeWins
            else do
              nbrFlag <- MV.read flagsArr (fromIntegral nbrInd)
              if nbrFlag == flag_CHOSEN
                then MV.write flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = MV.write flagsArr (fromIntegral selfInd) flag_CHOSEN
  for_ (0,numVerts) $ \ ndIx -> 
      when (vec UV.! ndIx == 1) $ do 
        let nds = nbrs gr (fromIntegral ndIx)
        loop (U.length nds) nds ndIx 0
  return flagsArr


-- MIS over a preexisting, filtered subgraph
------------------------------------------------------------
-- Right now this uses an IStructure because it's (temporarily) better at blocking gets:
maximalIndependentSet4 :: AdjacencyGraph -> (NatArray s Word8) -> Par d s (IStructure s Word8)
maximalIndependentSet4 gr@(AdjacencyGraph vvec evec) vertSubset = do
  let numVerts = U.length vvec
  -- Tradeoff: we use storage proportional to the ENTIRE graph.  If the subset is
  -- very small, this is silly and we could use a sparse representation:
  flagsArr <- newIStructure numVerts
  let       
      -- Here's the loop that scans through the neighbors of a node.
      loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          let nbrInd   = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          -- If we got to the end of the neighbors below us, then we are NOT disqualified:
          if nbrInd > selfInd
            then thisNodeWins
            else do
              nbrFlag <- ISt.get flagsArr (fromIntegral nbrInd)
              if nbrFlag == flag_CHOSEN
                then ISt.put_ flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = ISt.put_ flagsArr (fromIntegral selfInd) flag_CHOSEN
          
  NArr.forEach vertSubset $ \ ndIx _ -> 
        let nds = nbrs gr (fromIntegral ndIx) in
        loop (U.length nds) nds ndIx  0
  return flagsArr
