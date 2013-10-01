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
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-|

  The @lvish@ package provides a parallel programming model based on monotonically
  growing data structures.

  This module provides the core scheduler and basic control flow operations.  
  But to do anything useful you will need to import one of the data structure modules
  (@Data.LVar.*@).

  Here is a self-contained example that writes the same value to @num@
  twice and deterministically prints @4@ instead of raising an error, as
  it would if @num@ were a traditional IVar rather than an LVar. (You
  will need to compile using the @-XDataKinds@ extension.)

> {-# LANGUAGE DataKinds #-}
> import Control.LVish  -- Generic scheduler; works with any lattice.
> import Data.LVar.IVar -- The particular lattice in question.
> 
> p :: Par Det s Int
> p = do
>   num <- new
>   fork $ put num 4
>   fork $ put num 4
>   get num
> 
> main = do
>   print $ runPar $ p

 -}

-- This module reexports the default LVish scheduler, adding some type-level
-- wrappers to ensure propert treatment of determinism.
module Control.LVish
  (
    -- * CRITICAL OBLIGATIONS for the user: valid @Eq@ and total @Ord@
    {-| 
    We would like to tell you that if you're programming with Safe Haskell (@-XSafe@),
    that this library provides a formal guarantee that anything executed with `runPar` is
    guaranteed-deterministic.  Unfortunately, as of this release there is still one back-door
    that hasn't yet been closed.

    If an adverserial user defines invalid `Eq` instances (claiming objects are equal when they're
    not), or if they define a `compare` function that is not a /pure, total function/,
    and then they store those types within `LVar`s,
    then nondeterminism may leak out of a parallel `runPar` computation.

    In future releases, we will strive to require alternate, safe versions of `Eq` and
    `Ord` that are derived automatically by our library and by the GHC compiler.
    -}

    -- * Par computations and their parameters
    Par(), 
    Determinism(..), liftQD,
    
    -- * Basic control flow
    fork,
    yield, 
    runPar, runParIO,
--    runParIO_, runParLogged,
--    quiesceAll,
    
    -- * Various loop constructs
    parForL, parForSimple, parForTree, parForTiled, for_,

    -- * Synchronizing with handler pools
    L.HandlerPool(),    
    newPool, 
    withNewPool, withNewPool_, 
    quiesce, 
    
    forkHP,
    
    -- * Debug facilities and internal bits
    logStrLn, runParLogged, 
    LVar()
  ) where

import qualified Data.Foldable    as F 
import           Control.LVish.Internal
import           Control.LVish.DeepFrz.Internal (Frzn, Trvrsbl)
import qualified Control.LVish.SchedIdempotent as L
import           System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)

import           Prelude hiding (rem)
-- import GHC.Exts (Constraint)

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
-- throw 'NonDeterminismExn' on the thread that calls it, but if it returns without
-- exception then it always returns the same answer.
--
-- If the input computation is deterministic (`Det`), then @runParIO@ will return the
-- same result as `runPar`.  However, it is still conceivably useful for avoiding an
-- extra `unsafePerformIO` required inside the implementation of `runPar`.
-- 
-- In the future, full non-determinism may be allowed as a third setting.
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

-- | This is only used when compiled in debugging mode.  It atomically adds a string
-- onto an in-memory log.
logStrLn :: String -> Par d s ()
#ifdef DEBUG_LVAR
logStrLn = WrapPar . L.logStrLn
#else 
logStrLn _  = return ()
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
