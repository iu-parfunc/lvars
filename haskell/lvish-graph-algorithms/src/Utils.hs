{-# LANGUAGE CPP #-}

module Utils where

import Data.Set as Set

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
-- A simple FOLD operation.
------------------------------------------------------------------------------------------  

maxDegreeS :: AdjacencyGraph -> (ISet s NodeID) -> Par d s (MaxCounter s)
maxDegreeS gr component = do
  mc <- newMaxCounter 0 
  S.forEach component $ \ nd ->
    C.put mc (U.length$ nbrs gr nd)
  return mc


maxDegreeN :: AdjacencyGraph -> (NatArray s Word8) -> Par d s (MaxCounter s)
maxDegreeN gr component = do
  mc <- newMaxCounter 0 
  NArr.forEach component $ \ nd flg ->
    when (flg == 1) $
      C.put mc (U.length$ nbrs gr (fromIntegral nd))
  return mc


maxDegreeI :: AdjacencyGraph -> (IStructure s Word8) -> Par d s (MaxCounter s)
maxDegreeI gr component = do
  mc <- newMaxCounter 0
  -- INEFFICIENT: this attaches a handler to ALL ivars:
  ISt.forEachHP Nothing component $ \ nd flg -> do
    when (flg == 1) $ do
      let degree = U.length$ nbrs gr (fromIntegral nd)
      -- logStrLn$ " [maxDegreeI] Processing: "++show(nd,flg)++" with degree "++show degree
      C.put mc degree

  -- Better to just do this... wait for it to freeze and then loop.
  -- Problem is, we need to add wait-till-frozen!
  -- len <- ISt.getLength component
  -- parForTiled (0,len) $ \ nd ->
  --   when (flg == 1) $
  --     C.put mc (U.length$ nbrs gr (fromIntegral nd))

  return mc

------------------------------------------------------------------------------------------
-- A dummy per-node operation
------------------------------------------------------------------------------------------  

-- workEachNode :: (NatArray s Word8) -> (Word8 -> Par d s ()) -> Par d s (MaxCounter s)
workEachNode :: Word64 -> (NatArray s Word8) -> Par d s ()
workEachNode clocks component = do
  NArr.forEach component $ \ nd flg ->
    when (flg == 1) $ do
      liftIO$ wait_clocks clocks
      return ()

-- After freezing... this is a parallel loop, but doesn't use any monotonic data.
workEachVec :: Word64 -> UV.Vector Word8 -> Par d s ()
workEachVec clocks vec = do
  np <- liftIO$ getNumCapabilities
  -- for_ (0,UV.length vec) $ \ ix ->    
  -- parForTiled (np*4) (0,UV.length vec) $ \ ix ->
  -- parForSimple (0,UV.length vec) $ \ ix ->
  parForTree (0,UV.length vec) $ \ ix ->  
    let flg = vec UV.! ix in
    when (flg == 1) $ do
      liftIO$ wait_clocks clocks
      return ()

  -- Sequential version:  
  -- UV.forM_ vec $ \ flg ->
  --   when (flg == 1) $ do
  --     liftIO$ wait_clocks clocks
  --     return ()


workEachNodeI :: Word64 -> (IStructure s Word8) -> Par d s ()
workEachNodeI clocks component = do
  ISt.forEachHP Nothing component $ \ nd flg ->
    when (flg == 1) $ do
      liftIO$ wait_clocks clocks
      return ()

-- After freezing... this is a parallel loop, but doesn't use any monotonic data.
workEachVecMayb :: Word64 -> V.Vector (Maybe Word8) -> Par d s ()
workEachVecMayb clocks vec = do
  np <- liftIO$ getNumCapabilities
  -- for_ (0,UV.length vec) $ \ ix ->    
  -- parForTiled (np*4) (0,UV.length vec) $ \ ix ->
  -- parForSimple (0,UV.length vec) $ \ ix ->
  parForTree (0, V.length vec) $ \ ix ->  
    let flg = vec V.! ix in
    when (flg == Just 1) $ do
      liftIO$ wait_clocks clocks
      return ()


--------------------------------------------------------------------------------
-- Misc helpers
--------------------------------------------------------------------------------

{-# INLINE forVec #-}
-- | Simple for-each loops over vector elements.
forVec :: U.Unbox a => U.Vector a -> (a -> Par d s ()) -> Par d s ()
forVec vec fn = loop 0 
  where
    len = U.length vec
    loop i | i == len = return ()
           | otherwise = fn (U.unsafeIndex vec i) >>
                         loop (i+1)

type ParFor d s = (Int,Int) -> (Int -> Par d s ()) -> Par d s ()

