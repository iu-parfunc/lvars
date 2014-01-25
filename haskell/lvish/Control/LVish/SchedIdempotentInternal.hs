{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns, BangPatterns #-}
{-# LANGUAGE RecursiveDo #-}

module Control.LVish.SchedIdempotentInternal (
  State(logger, no), initLogger,
  new, number, next, pushWork, nullQ, yieldWork, currentCPU, setStatus, await, prng
  ) where


import Prelude
import Control.Monad
import Control.Concurrent
import Control.DeepSeq
import Control.Applicative
import Data.IORef 
import GHC.Conc
import System.Random (StdGen, mkStdGen)
import System.IO (stdout)
import Text.Printf

import qualified Control.LVish.Logging as L

#ifdef CHASE_LEV
#warning "Compiling with Chase-Lev work-stealing deque"

import Data.Concurrent.Deque.ChaseLev as CL

type Deque a = CL.ChaseLevDeque a
newDeque = CL.newQ
pushMine = CL.pushL
popMine  = CL.tryPopL
popOther = CL.tryPopR 
pushYield = pushMine -- for now...  
nullQ = CL.nullQ

#else
#warning "Compiling with non-scalable deque."
------------------------------------------------------------------------------
-- A nonscalable deque for work-stealing
------------------------------------------------------------------------------

type Deque a = IORef [a]

-- | Create a new local work deque
newDeque :: IO (Deque a)
newDeque = newIORef []

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

nullQ :: Deque a -> IO Bool
nullQ deque = do
  ls <- readIORef deque
  return $! null ls


-- | Add low-priority work to a thread's own work deque
pushYield :: Deque a -> a -> IO ()
pushYield deque t = 
  atomicModifyIORef deque $ \ts -> (ts++[t], ()) 

-- | Take work from a different thread's work deque
popOther :: Deque a -> IO (Maybe a)
popOther = popMine

#endif

------------------------------------------------------------------------------
-- A scheduling framework
------------------------------------------------------------------------------

-- All the state relevant to a single worker thread
data State a s = State
    { no       :: {-# UNPACK #-} !Int, -- ^ The number of this worker
      numWorkers :: Int,               -- ^ Total number of workers in this runPar
      prng     :: IORef StdGen,        -- ^ core-local random number generation
      status   :: IORef s,             -- ^ A thread-local flag
      workpool :: Deque a,             -- ^ The thread-local work deque
      idle     :: IORef [MVar Bool],   -- ^ global list of idle workers
      states   :: [State a s],         -- ^ global list of all worker states.
      logger   :: IORef (Maybe L.Logger)
        -- ^ The Logger object used by the current Par session.  (This should not
        -- change during runtime, it is mutable only to support deferred
        -- initialization.)
    }
    
-- | Process the next item on the work queue or, failing that, go into
-- work-stealing mode.
{-# INLINE next #-}
next :: State a s -> IO (Maybe a)
next state@State{ workpool } = do
  e <- popMine workpool
  case e of
    Nothing -> steal state
    Just t  -> return e

-- RRN: Note -- NOT doing random work stealing breaks the traditional
-- Cilk time/space bounds if one is running strictly nested (series
-- parallel) programs.

-- | Attempt to steal work or, failing that, give up and go idle (and then wake back
-- up and keep stealing).
--     
--   This function does NOT return until the complete runPar session is complete (all
--   workers idle).
steal :: State a s -> IO (Maybe a)
steal State{ idle, states, no=my_no, numWorkers, logger } = do
  chatter logger $ "!cpu "++show my_no++" stealing" 
  go states
  where
    -- After a failed sweep, go idle:
    go [] = do m <- newEmptyMVar
               r <- atomicModifyIORef idle $ \is -> (m:is, is)
               if length r == numWorkers - 1
                  then do
                     chatter logger $ printf "!cpu %d initiating shutdown\n" my_no
                     mapM_ (\m -> putMVar m True) r
                     return Nothing
                  else do
                    chatter logger $ printf "!cpu %d going idle...\n" my_no
                    done <- takeMVar m
                    if done
                       then do
                         chatter logger $ printf "!cpu %d shutting down\n" my_no
                         return Nothing
                       else do
                         chatter logger $ printf "!cpu %d woken up\n" my_no
                         go states
    go (x:xs)
      | no x == my_no = go xs
      | otherwise     = do
         r <- popOther (workpool x)
         case r of
           Just t  -> do
             chatter logger $ printf "cpu %d got work from cpu %d\n" my_no (no x)
             return r
           Nothing -> go xs

-- | If any worker is idle, wake one up and give it work to do.
pushWork :: State a s -> a -> IO ()
-- TODO: If we're really going to do wakeup on every push we could consider giving
-- the formerly-idle worker the work item directly and thus avoid touching the deque.
pushWork State { workpool, idle, logger, no } t = do
  chatter logger $ "Starting pushWork on worker "++show no
  pushMine workpool t
  idles <- readIORef idle
  when (not (null idles)) $ do
    r <- atomicModifyIORef idle (\is -> case is of
                                          [] -> ([], return ())
                                          (i:is) -> (is, putMVar i False))
    r -- wake one up
        
yieldWork :: State a s -> a -> IO ()
yieldWork State { workpool } t = 
  pushYield workpool t -- AJT: should this also wake an idle thread?

-- | Create a new set of scheduler states.
new :: Int -> s -> IO [State a s]
new numWorkers s = do
  idle   <- newIORef []
  logger <- newIORef Nothing
  let mkState states i = do 
        workpool <- newDeque
        status   <- newIORef s
        prng     <- newIORef $ mkStdGen i
        return State { no = i, workpool, idle, status, states, prng, logger, numWorkers }
  rec states <- forM [0..(numWorkers-1)] $ mkState states
  return states

-- | Takes a full set of worker states and correspoding threadIds and initializes the
-- loggers.
initLogger :: [State a s] -> [ThreadId] -> (Int,Int) -> [L.OutDest] -> Bool -> IO ()
initLogger [] _ _ _ _ = error "initLogger: cannot take empty list of workers"
initLogger queues@(hd:_) tids bounds outDests debugScheduling
  | len1 /= len2 = error "initLogger: length of arguments did not match"
  | otherwise = do
    lgr <- L.newLogger bounds outDests
              (if debugScheduling then waitAll else L.DontWait)
    -- lgr <- L.newLogger Nothing (L.WaitNum len1 countIdle)
    L.logOn lgr (L.StrMsg 1 " [dbg-lvish] Initializing Logger... ")
    -- Setting one of them sets all of them -- this field is shared:
    writeIORef (logger hd) (Just lgr)
    -- TODO: ASSERT that they are all actually the same IORef?
    return ()
 where
   waitAll = (L.WaitTids tids (pollDeques queues))
   
   len1 = length queues
   len2 = length tids
   countIdle = do ls <- readIORef (idle hd)
                  return $! length ls
   pollDeques [] = return True
   pollDeques (h:t) = do b <- nullQ (workpool h)
                         if b then pollDeques t
                              else return False

number :: State a s -> Int
number State { no } = no

setStatus :: State a s -> s -> IO ()
setStatus State { status } s = writeIORef status s

-- This is a hard-spinning busy-wait.
await :: State a s -> (s -> Bool) -> IO ()
await State { states, logger, no=no1 } p = 
  let awaitOne state@(State { status, no=no2 }) = do
        cur <- readIORef status
        unless (p cur) $ do
          mlgr <- readIORef logger
          case mlgr of
            Nothing -> return ()
            Just lgr -> L.logOn lgr (L.StrMsg 7 (" [dbg-lvish] busy-waiting on worker "++show no1++
                                                 ", for status to change on worker "++show no2))
          awaitOne state
  in mapM_ awaitOne states

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


chatter :: IORef (Maybe L.Logger) -> String -> IO ()
-- chatter s = putStrLn s
-- chatter s = printf "%s\n" s
chatter ref s = do 
  mlg <- readIORef ref
  case mlg of 
    Nothing -> return ()
    Just lg -> L.logOn lg (L.StrMsg 7 s)
