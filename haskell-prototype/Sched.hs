{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns, BangPatterns #-}

module Sched(Queue, new, next, pushWork, yieldWork, currentCPU) where

import Prelude
import Control.Monad
import Control.Concurrent
import Control.DeepSeq
import Control.Applicative
import Data.IORef 
import GHC.Conc

------------------------------------------------------------------------------
-- A nonscalable deque for work-stealing
------------------------------------------------------------------------------

type Deque a = IORef [a]

-- | Add work to a thread's own work deque
pushMine :: Deque a -> a -> IO ()
pushMine deque t = 
  atomicModifyIORef deque $ \ts -> (t:ts, ())
                                   
-- | Take work from a thread's own work deque
popMine :: Deque a -> IO (Maybe a)
popMine deque = do
  atomicModifyIORef deque $ \ts ->
    case ts of
      []      -> ([], Nothing)
      (t:ts') -> (ts', Just t)

-- | Add low-priority work to a thread's own work deque
pushYield :: Deque a -> a -> IO ()
pushYield deque t = 
  atomicModifyIORef deque $ \ts -> (ts++[t], ()) 

-- | Take work from a different thread's work deque
popOther :: Deque a -> IO (Maybe a)
popOther = popMine

------------------------------------------------------------------------------
-- A scheduling framework
------------------------------------------------------------------------------

data Queue a = Queue
    { no       :: {-# UNPACK #-} !Int,
      workpool :: IORef [a],
      idle     :: IORef [MVar Bool],
      scheds   :: [Queue a] -- Global list of all per-thread workers.
    }

-- | Process the next item on the work queue or, failing that, go into
-- work-stealing mode.
{-# INLINE next #-}
next :: Queue a -> IO (Maybe a)
next queue@Queue{ workpool } = do
  e <- popMine workpool
  case e of
    Nothing -> steal queue
    Just t  -> return e

-- RRN: Note -- NOT doing random work stealing breaks the traditional
-- Cilk time/space bounds if one is running strictly nested (series
-- parallel) programs.

-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Queue a -> IO (Maybe a)
steal Queue{ idle, scheds, no=my_no } = do
  -- printf "cpu %d stealing\n" my_no
  go scheds
  where
    go [] = do m <- newEmptyMVar
               r <- atomicModifyIORef idle $ \is -> (m:is, is)
               if length r == numCapabilities - 1
                  then do
                     -- printf "cpu %d initiating shutdown\n" my_no
                     mapM_ (\m -> putMVar m True) r
                     return Nothing
                  else do
                    done <- takeMVar m
                    if done
                       then do
                         -- printf "cpu %d shutting down\n" my_no
                         return Nothing
                       else do
                         -- printf "cpu %d woken up\n" my_no
                         go scheds
    go (x:xs)
      | no x == my_no = go xs
      | otherwise     = do
         r <- popOther (workpool x)
         case r of
           Just t  -> do
              -- printf "cpu %d got work from cpu %d\n" my_no (no x)
             return r
           Nothing -> go xs

-- | If any worker is idle, wake one up and give it work to do.
pushWork :: Queue a -> a -> IO ()
pushWork Queue { workpool, idle } t = do
  pushMine workpool t
  idles <- readIORef idle
  when (not (null idles)) $ do
    r <- atomicModifyIORef idle (\is -> case is of
                                          [] -> ([], return ())
                                          (i:is) -> (is, putMVar i False))
    r -- wake one up
        
yieldWork :: Queue a -> a -> IO ()
yieldWork Queue { workpool } t = 
  pushYield workpool t -- AJT: should this also wake an idle thread?

new :: Int -> IO [Queue a]
new n = do
  workpools <- replicateM n $ newIORef []
  idle <- newIORef []
  let states = [ Queue { no=x, workpool=wp, idle, scheds=states } 
               | (x,wp) <- zip [0..] workpools ]
  return states

-- | the CPU executing the current thread (0 if not supported)
currentCPU :: IO Int
currentCPU = 
#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
  --
  -- Note: GHC 7.1.20110301 is required for this to work, because that
  -- is when threadCapability was added.
  --
      do 
        tid <- myThreadId
        (main_cpu, _) <- threadCapability tid
        return main_cpu
#else
  --
  -- Lacking threadCapability, we always pick CPU #0 to run the main
  -- thread.  If the current thread is not running on CPU #0, this
  -- will require some data to be shipped over the memory bus, and
  -- hence will be slightly slower than the version above.
  --
  return 0
#endif