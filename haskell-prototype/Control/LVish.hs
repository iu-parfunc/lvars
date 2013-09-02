{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}  -- For 'Determinism'
-- {-# LANGUAGE ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}


-- | A module that reexports the default LVish scheduler, adding some type-level
-- wrappers to ensure propert treatment of determinism.

module Control.LVish
  (
    -- * Basic types and accessors:
    LVar(), state, L.HandlerPool(), Par(), 
    Determinism(..), liftQ,
    
    -- * Safe, deterministic operations:
    yield, newPool, fork, forkHP,
    runPar, runParIO, runParIO_, runParLogged,
--    unsafeRunThenFreeze, runParThenFreezeIO, runParThenFreezeIO2,
    withNewPool, withNewPool_,
    
    -- * Quasi-deterministic operations:
    quiesce, quiesceAll,
    
    -- * Interfaces for generic operations
    LVarData1(..),
    
    -- * Generally useful combinators
    parForL, parForSimple, parForTree, parForTiled, for_,
    
    -- * Debug facilities
    logStrLn
  ) where

import           Control.Applicative
import           Control.LVish.Internal
import           Control.LVish.DeepFrz.Internal (Frzn)
import qualified Control.LVish.SchedIdempotent as L
import           System.IO.Unsafe (unsafePerformIO)

-- import GHC.Exts (Constraint)

--------------------------------------------------------------------------------
-- Inline *everything*, because these are just wrappers:
{-# INLINE liftQ #-}
{-# INLINE yield #-}
{-# INLINE newPool #-}
{-# INLINE runParIO #-}
{-# INLINE runPar #-}
-- {-# INLINE runParThenFreeze #-}
{-# INLINE fork #-}
{-# INLINE quiesce #-}
--------------------------------------------------------------------------------

-- | It is always safe to lift a deterministic computation to a quasi-determinism one.
liftQ :: Par Det s a -> Par QuasiDet s a
liftQ (WrapPar p) = (WrapPar p)

yield :: Par d s ()
yield = WrapPar L.yield

quiesce :: L.HandlerPool -> Par d s ()
quiesce = WrapPar . L.quiesce

-- | A global barrier.
quiesceAll :: Par d s ()
quiesceAll = WrapPar L.quiesceAll

fork :: Par d s () -> Par d s ()
fork (WrapPar f) = WrapPar$ L.fork f

forkHP :: Maybe L.HandlerPool -> Par d s () -> Par d s ()
forkHP mh (WrapPar f) = WrapPar$ L.forkHP mh f

newPool :: Par d s L.HandlerPool
newPool = WrapPar L.newPool

withNewPool :: (L.HandlerPool -> Par d s a) -> Par d s (a, L.HandlerPool)
withNewPool f = WrapPar $ L.withNewPool $ unWrapPar . f

withNewPool_ :: (L.HandlerPool -> Par d s ()) -> Par d s L.HandlerPool
withNewPool_ f = WrapPar $ L.withNewPool_ $ unWrapPar . f

-- | If the input computation is quasi-deterministic, this may throw
-- 'NonDeterminismExn' on the thread that calls it.
runParIO :: (forall s . Par d s a) -> IO a
runParIO (WrapPar p) = L.runParIO p 

-- | Useful ONLY for timing.
runParIO_ :: (Par d s a) -> IO ()
runParIO_ (WrapPar p) = L.runParIO p >> return ()

-- | Useful for debugging.  Returns debugging logs, in realtime order, in addition to
-- the final result.
runParLogged :: (forall s . Par d s a) -> IO ([String],a)
runParLogged (WrapPar p) = L.runParLogged p 

runPar :: (forall s . Par Det s a) -> a
runPar (WrapPar p) = L.runPar p 


logStrLn :: String -> Par d s ()
#ifdef DEBUG_LVAR
logStrLn = WrapPar . L.logStrLn
#else 
logStrLn _  = return ()
{-# INLINE logStrLn #-}
#endif


------------------------------------------------------------------------------
-- Interface for generic LVar handling
------------------------------------------------------------------------------

-- class Traversable f => LVarData1 (f :: * -> *) where

-- | A class representing monotonic data types that take one type
-- parameter as well as an `s` parameter for session safety.
-- 
--   TODO: if there is a Par class, it needs to be a superclass of this.
class LVarData1 (f :: * -> * -> *) where
  -- | The a frozen LVar provides a complete picture of the contents:
  -- e.g. a whole set instead of one element, or the full/empty
  -- information for an IVar, instead of just the payload.
  
  -- type LVCtxt (f :: * -> * -> *) (s :: *) (a :: *) :: Constraint
  --  I was not able to get abstracting over the constraints to work.

  freeze :: -- LVCtxt f s a =>
            f s a -> Par QuasiDet s (f Frzn a)
  newBottom :: Ord a => Par d s (f s a)

  -- NOTE: there is currently no way to assert any constraints on the
  -- RANGE of a type function:

  -- | 
  traverseSnap :: (a -> Par d s b) -> f Frzn a -> Par d s (f Frzn b)
  -- TODO: use constraint kinds to constrain 'b'.

  -- foldSnap :: (a -> b -> Par d s b) -> b -> f s a -> Par d s b

  -- What else?
  -- Merge op?


-- This gets messy if we try to handle several Kinds:

-- | Just like LVarData1 but for type constructors of kind `*`.
class LVarData0 (t :: *) where
  -- | This associated type models a picture of the "complete" contents of the data:
  -- e.g. a whole set instead of one element, or the full/empty information for an
  -- IVar, instead of just the payload.
  type Snapshot0 t
  freeze0 :: t -> Par QuasiDet s (Snapshot0 t)
  newBottom0 :: Par d s t


--------------------------------------------------------------------------------
-- Extras
--------------------------------------------------------------------------------

{-# INLINE parForL #-}
-- | Left-biased parallel for loop.  As worker threads beyond the first are added,
-- this hews closer to the sequential iteration order than an unbiased parallel loop.
--
-- Takes a range as inclusive-start, exclusive-end.
parForL :: (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForL (start,end) body | start > end = error$"parForL: start is greater than end: "++show (start,end)
parForL (start,end) body = do
  -- logStrLn$ " initial iters: "++show (end-start)
  loop 0 (end - start) 1
 where
   loop offset remain chunk
     | remain <= 0     = return () 
     | remain <= chunk = parForSimple (offset, offset+remain) body
     | otherwise       = do
         let nxtstrt = offset+chunk
         -- logStrLn$ "loop:  .. "++show (offset, remain, chunk)
         fork$ parForSimple (offset, nxtstrt) body
         loop nxtstrt (remain-chunk) (2*chunk)

   -- forSlice = parForSimple
   -- forSlice = parForTree
   forSlice = parForL

{-# INLINE parForSimple #-}
-- | The least-sophisticated form of parallel loop.  Fork iterations one at a time.
parForSimple :: (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForSimple range fn = do
  for_ range $ \i -> fork (fn i) 

-- | Divide the iteration space recursively, but ultimately run every iteration in
-- parallel.  That is, the loop body is permitted to block on other iterations.
parForTree :: (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForTree (start,end) body | start > end = error$"parForTree: start is greater than end: "++show (start,end)
parForTree (start,end) body = do
  loop 0 (end - start)
 where
   loop offset remain 
     | remain == 1     = body offset
     | otherwise       = do
         let (half,rem) = remain `quotRem` 2
         fork$ loop offset half
         loop (offset+half) (half+rem)


-- | Split the work into a number of tiles, and fork it in a tree topology.
parForTiled :: Int -> (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForTiled tiles (start,end) body = do 
  loop 0 (end - start) tiles
 where
   loop offset remain tiles
     | remain == 1     = body offset
     | tiles  == 1     = for_ (offset,offset+remain) body
     | otherwise       = do
         let (half,rem)   = remain `quotRem` 2
             (halfT,remT) = tiles `quotRem` 2
         fork$ loop offset half halfT
         loop (offset+half) (half+rem) (halfT+remT)
