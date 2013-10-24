 {-# LANGUAGE Trustworthy #-}
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
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | An internal module simply reexported by Control.LVish.

module Control.LVish.Basics where

import qualified Data.Foldable    as F
import           Control.Exception (Exception)
import           Control.LVish.Internal
import           Control.LVish.DeepFrz.Internal (Frzn, Trvrsbl)
import qualified Control.LVish.SchedIdempotent as L
import           Control.LVish.Types
import           System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)

import           Prelude hiding (rem)
-- import GHC.Exts (Constraint)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Inline *everything*, because these are just wrappers:
{-# INLINE liftQD #-}
{-# INLINE yield #-}
{-# INLINE newPool #-}
{-# INLINE runParIO #-}
{-# INLINE runPar #-}
--{-# INLINE runParThenFreeze #-}
{-# INLINE fork #-}
{-# INLINE quiesce #-}
--------------------------------------------------------------------------------

-- | It is always safe to lift a deterministic computation to a
-- quasi-deterministic one.
liftQD :: Par Det s a -> Par QuasiDet s a
liftQD (WrapPar p) = (WrapPar p)

-- | Cooperatively schedule other threads.
yield :: Par d s ()
yield = WrapPar L.yield

-- | Block until a handler pool is quiescent, i.e., until all
-- associated parallel computations have completed.
quiesce :: L.HandlerPool -> Par d s ()
quiesce = WrapPar . L.quiesce

-- | A global barrier.  Wait for all unblocked, active threads of work in the system
-- to complete, and then proceed after that point.
quiesceAll :: Par d s ()
quiesceAll = WrapPar L.quiesceAll

-- | Execute a computation in parallel.
fork :: Par d s () -> Par d s ()
fork (WrapPar f) = WrapPar$ L.fork f

-- | A version of `fork` that also allows the forked computation to be tracked in a
-- `HandlerPool`, that enables the programmer to synchronize on the completion of the
-- child computation.  But be careful; this does not automatically wait for
-- all downstream forked computations (transitively).
forkHP :: Maybe L.HandlerPool -> Par d s () -> Par d s ()
forkHP mh (WrapPar f) = WrapPar$ L.forkHP mh f

-- | Create a new pool that can be used to synchronize on the completion of all
-- parallel computations associated with the pool.
newPool :: Par d s L.HandlerPool
newPool = WrapPar L.newPool

-- | Execute a Par computation in the context of a fresh handler pool.
withNewPool :: (L.HandlerPool -> Par d s a) -> Par d s (a, L.HandlerPool)
withNewPool f = WrapPar $ L.withNewPool $ unWrapPar . f

-- | Execute a Par computation in the context of a fresh handler pool, while
-- ignoring the result of the computation.
withNewPool_ :: (L.HandlerPool -> Par d s ()) -> Par d s L.HandlerPool
withNewPool_ f = WrapPar $ L.withNewPool_ $ unWrapPar . f

-- | If the input computation is quasi-deterministic (`QuasiDet`), then this may
-- throw a `LVishException` nondeterministically on the thread that calls it, but if
-- it returns without exception then it always returns the same answer.
--
-- If the input computation is deterministic (`Det`), then @runParIO@ will return the
-- same result as `runPar`.  However, `runParIO` is still possibly useful for
-- avoiding an extra `unsafePerformIO` required inside the implementation of
-- `runPar`.
-- 
-- In the future, /full/ nondeterminism may be allowed as a third setting beyond
-- `Det` and `QuasiDet`.
runParIO :: (forall s . Par d s a) -> IO a
runParIO (WrapPar p) = L.runParIO p 

-- | Useful ONLY for timing.
runParIO_ :: (Par d s a) -> IO ()
runParIO_ (WrapPar p) = L.runParIO p >> return ()

-- | Useful for debugging.  Returns debugging logs, in realtime order, in addition to
-- the final result.
runParLogged :: (forall s . Par d s a) -> IO ([String],a)
runParLogged (WrapPar p) = L.runParLogged p 

-- | If a computation is guaranteed-deterministic, then `Par` becomes a dischargeable
-- effect.  This function will create new worker threads and do the work in parallel,
-- returning the final result.
--
-- (For now there is no sharing of workers with repeated invocations; so
-- keep in mind that @runPar@ is an expensive operation. [2013.09.27])
runPar :: (forall s . Par Det s a) -> a
runPar (WrapPar p) = L.runPar p 

-- logStrLn :: String -> Par d s ()
-- #ifdef DEBUG_LVAR
-- logStrLn = WrapPar . L.logStrLn
-- #else 
-- logStrLn _  = return ()
-- {-# INLINE logStrLn #-}
-- #endif

-- | Log a line of debugging output.  This is only used when *compiled* in debugging
-- mode.  It atomically adds a string onto an in-memory log.
-- 
-- The provided `Int`, is the "debug level" associated with the message, 1-5.  One is
-- the least verbose, and five is the most.  When debugging, the user can control the
-- debug level by setting the env var DEBUG, e.g. @DEBUG=5@.
logDbgLn :: Int -> String -> Par d s ()
#ifdef DEBUG_LVAR
logDbgLn n = WrapPar . L.liftIO . L.logLnAt_ n 
#else 
logDbgLn _ _  = return ()
{-# INLINE logStrLn #-}
#endif



--------------------------------------------------------------------------------
-- Extras
--------------------------------------------------------------------------------

{-# INLINE parForL #-}
-- | Left-biased parallel for loop.  As worker threads beyond the first are added,
-- this hews closer to the sequential iteration order than an unbiased parallel loop.
--
-- Takes a range as inclusive-start, exclusive-end.
parForL :: (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForL (start,end) _ | start > end = error$"parForL: start is greater than end: "++show (start,end)
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

{-# INLINE parForSimple #-}
-- | The least-sophisticated form of parallel loop.  Fork iterations one at a time.
parForSimple :: (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForSimple range fn = do
  for_ range $ \i -> fork (fn i) 

-- | Divide the iteration space recursively, but ultimately run every iteration in
-- parallel.  That is, the loop body is permitted to block on other iterations.
parForTree :: (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForTree (start,end) _
  | start > end = error$"parForTree: start is greater than end: "++show (start,end)
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
parForTiled otiles (start,end) body = do 
  loop 0 (end - start) otiles
 where
   loop offset remain tiles
     | remain == 1     = body offset
     | tiles  == 1     = for_ (offset,offset+remain) body
     | otherwise       = do
         let (half,rem)   = remain `quotRem` 2
             (halfT,remT) = tiles `quotRem` 2
         fork$ loop offset half halfT
         loop (offset+half) (half+rem) (halfT+remT)


-- | A simple for loop for numeric ranges (not requiring deforestation
-- optimizations like `forM`).  Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => (Int, Int) -> (Int -> m ()) -> m ()
for_ (start, end) _fn | start > end = error "for_: start is greater than end"
for_ (start, end) fn = loop start
  where
  loop !i | i == end  = return ()
          | otherwise = do fn i; loop (i+1)
