{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables, RankNTypes #-}

-- | To make it easier to build (multithreaded) tests

module TestHelpers
 ( 
   -- * Testing parameters
   numElems, getNumAgents, producerRatio,

   -- * Utility for controlling the number of threads used by generated tests.
   setTestThreads,

   -- * Test initialization, reading common configs
   stdTestHarness, 
   
   -- * A replacement for defaultMain that uses a 1-thread worker pool 
   defaultMainSeqTests,

   -- * Misc utilities
   nTimes, for_, forDown_, assertOr, timeOut, assertNoTimeOut, splitRange, timeit,
   theEnv,
   
   -- timeOutPure, 
   exceptionOrTimeOut, allowSomeExceptions, assertException
 )
 where 

import Control.Monad
import Control.Exception
--import Control.Concurrent
--import Control.Concurrent.MVar
import GHC.Conc
import Data.IORef
import Data.Word
import Data.Time.Clock
import Data.List (isInfixOf, intersperse, nub)
import Text.Printf
import Control.Concurrent (forkOS, forkIO, ThreadId)
-- import Control.Exception (catch, SomeException, fromException, bracket, AsyncException(ThreadKilled))
import Control.Exception (bracket)
import System.Environment (withArgs, getArgs, getEnvironment)
import System.IO (hFlush, stdout, stderr, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)
import System.Exit
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit  (hUnitTestToTests)

import Data.Monoid (mappend, mempty)
import Test.Framework.Runners.Console (interpretArgs, defaultMainWithOpts)
import Test.Framework.Runners.Options (RunnerOptions'(..))
import Test.Framework.Options (TestOptions'(..))
import Test.HUnit as HU

import Debug.Trace (trace)

--------------------------------------------------------------------------------


#if __GLASGOW_HASKELL__ >= 704
import GHC.Conc (getNumCapabilities, setNumCapabilities, getNumProcessors)
#else
import GHC.Conc (numCapabilities)
getNumCapabilities :: IO Int
getNumCapabilities = return numCapabilities

setNumCapabilities :: Int -> IO ()
setNumCapabilities = error "setNumCapabilities not supported in this older GHC!  Set NUMTHREADS and +RTS -N to match."

getNumProcessors :: IO Int
getNumProcessors = return 1 
#endif    

theEnv :: [(String, String)]
theEnv = unsafePerformIO getEnvironment

----------------------------------------------------------------------------------------------------
-- TODO: In addition to setting these parameters from environment
-- variables, it would be nice to route all of this through a
-- configuration record, so that it can be changed programmatically.

-- How many elements should each of the tests pump through the queue(s)?
numElems :: Maybe Int
numElems = case lookup "NUMELEMS" theEnv of 
             Nothing  -> Nothing -- 100 * 1000 -- 500000
             Just str -> warnUsing ("NUMELEMS = "++str) $ 
                         Just (read str)

forkThread :: IO () -> IO ThreadId
forkThread = case lookup "OSTHREADS" theEnv of 
               Nothing -> forkIO
               Just x -> warnUsing ("OSTHREADS = "++x) $ 
                 case x of 
                   "0"     -> forkIO
                   "False" -> forkIO
                   "1"     -> forkOS
                   "True"  -> forkOS
                   oth -> error$"OSTHREAD environment variable set to unrecognized option: "++oth

-- | How many communicating agents are there?  By default one per
-- thread used by the RTS.
getNumAgents :: IO Int
getNumAgents = case lookup "NUMAGENTS" theEnv of 
                Nothing  -> getNumCapabilities
                Just str -> warnUsing ("NUMAGENTS = "++str) $ 
                            return (read str)

-- | It is possible to have imbalanced concurrency where there is more
-- contention on the producing or consuming side (which corresponds to
-- settings of this parameter less than or greater than 1).
producerRatio :: Double
producerRatio = case lookup "PRODUCERRATIO" theEnv of 
                 Nothing  -> 1.0
                 Just str -> warnUsing ("PRODUCERRATIO = "++str) $ 
                             read str

warnUsing :: String -> a -> a
warnUsing str a = trace ("  [Warning]: Using environment variable "++str) a


-- | Dig through the test constructors to find the leaf IO actions and bracket them
--   with a thread-setting action.
setTestThreads :: Int -> HU.Test -> HU.Test
setTestThreads nm tst = loop False tst
 where
   loop flg x = 
    case x of
      TestLabel lb t2 -> TestLabel (decor flg lb) (loop True t2)
      TestList ls -> TestList (map (loop flg) ls)
      TestCase io -> TestCase (bracketThreads nm io)

   -- We only need to insert the numcapabilities in the description string ONCE:
   decor False lb = "N"++show nm++"_"++ lb
   decor True  lb = lb

   bracketThreads :: Int -> IO a -> IO a
   bracketThreads n act =
     bracket (getNumCapabilities)
             setNumCapabilities
             (\_ -> do dbgPrint 1 ("\n   [Setting # capabilities to "++show n++" before test] \n")
                       setNumCapabilities n
                       act)

-- | Repeat a group of tests while varying the number of OS threads used.  Also,
-- read configuration info.
--
-- WARNING: uses setNumCapabilities.
stdTestHarness :: (IO Test) -> IO ()
stdTestHarness genTests = do 
  numAgents <- getNumAgents 
  putStrLn$ "Running with numElems "++show numElems++" and numAgents "++ show numAgents
  putStrLn "Use NUMELEMS, NUMAGENTS, NUMTHREADS to control the size of this benchmark."
  args <- getArgs

  np <- getNumProcessors
  putStrLn $"Running on a machine with "++show np++" hardware threads."

  -- We allow the user to set this directly, because the "-t" based regexp selection
  -- of benchmarks is quite limited.
  let all_threads = case lookup "NUMTHREADS" theEnv of
                      Just str -> [read str]
                      Nothing -> nub [1, 2, np `quot` 2, np, 2*np ]
  putStrLn $"Running tests for these thread settings: "  ++show all_threads
  all_tests <- genTests 

  -- Don't allow concurent tests (the tests are concurrent!):
  withArgs (args ++ ["-j1","--jxml=test-results.xml"]) $ do 

    -- Hack, this shouldn't be necessary, but I'm having problems with -t:
    tests <- case all_threads of
              [one] -> do cap <- getNumCapabilities
                          unless (cap == one) $ setNumCapabilities one
                          return all_tests
              _ -> return$ TestList [ setTestThreads n all_tests | n <- all_threads ]
    TF.defaultMain$ hUnitTestToTests tests


----------------------------------------------------------------------------------------------------
-- DEBUGGING
----------------------------------------------------------------------------------------------------

-- | Debugging flag shared by all accelerate-backend-kit modules.
--   This is activated by setting the environment variable DEBUG=1..5
dbg :: Int
dbg = case lookup "DEBUG" theEnv of
       Nothing  -> defaultDbg
       Just ""  -> defaultDbg
       Just "0" -> defaultDbg
       Just s   ->
         trace (" ! Responding to env Var: DEBUG="++s)$
         case reads s of
           ((n,_):_) -> n
           [] -> error$"Attempt to parse DEBUG env var as Int failed: "++show s

defaultDbg :: Int
defaultDbg = 0

-- | Print if the debug level is at or above a threshold.
dbgPrint :: Int -> String -> IO ()
dbgPrint lvl str = if dbg < lvl then return () else do
--    hPutStrLn stderr str
    -- hPrintf stderr str 
    -- hFlush stderr
    printf str
    hFlush stdout

dbgPrintLn :: Int -> String -> IO ()
dbgPrintLn lvl str = dbgPrint lvl (str++"\n")


------------------------------------------------------------------------------------------
-- Misc Helpers
------------------------------------------------------------------------------------------

-- | Ensure that executing an action returns an exception
-- containing one of the expected messages.
assertException  :: [String] -> IO a -> IO ()
assertException msgs action = do
 x <- catch (do action; return Nothing) 
            (\e -> do putStrLn $ "Good.  Caught exception: " ++ show (e :: SomeException)
                      return (Just $ show e))
 case x of 
  Nothing -> HU.assertFailure "Failed to get an exception!"
  Just s -> 
   if  any (`isInfixOf` s) msgs
   then return () 
   else HU.assertFailure $ "Got the wrong exception, expected one of the strings: "++ show msgs
        ++ "\nInstead got this exception:\n  " ++ show s

-- | For testing quasi-deterministic programs: programs that always
-- either raise a particular exception or produce a particular answer.
allowSomeExceptions :: [String] -> IO a -> IO (Either SomeException a)
allowSomeExceptions msgs action = do
 catch (do a <- action; evaluate a; return (Right a))
       (\e ->
         let estr = show e in
         if  any (`isInfixOf` estr) msgs
          then do when True $ -- (dbgLvl>=1) $
                    putStrLn $ "Caught allowed exception: " ++ show (e :: SomeException)
                  return (Left e)
          else do HU.assertFailure $ "Got the wrong exception, expected one of the strings: "++ show msgs
                    ++ "\nInstead got this exception:\n  " ++ show estr
                  error "Should not reach this..."
       )

exceptionOrTimeOut :: Show a => Double -> [String] -> IO a -> IO ()
exceptionOrTimeOut time msgs action = do
  x <- timeOut time $
       allowSomeExceptions msgs action
  case x of
    Just (Right _val) -> HU.assertFailure "exceptionOrTimeOut: action returned successfully!" 
    Just (Left _exn)  -> return () -- Error, yay!
    Nothing           -> return () -- Timeout.

-- | Simple wrapper around `timeOut` that throws an error if timeOut occurs.
assertNoTimeOut :: Show a => Double -> IO a -> IO a
assertNoTimeOut t a = do
  m <- timeOut t a
  case m of
    Nothing -> do HU.assertFailure$ "assertNoTimeOut: thread failed or timeout occurred after "++show t++" seconds"
                  error "Should not reach this #2"
    Just a  -> return a  

-- | Time-out an IO action by running it on a separate thread, which is killed when
-- the timer (in seconds) expires.  This requires that the action do allocation, otherwise it will
-- be non-preemptable.
timeOut :: Show a => Double -> IO a -> IO (Maybe a)
timeOut interval act = do
  result <- newIORef Nothing
  tid <- forkIO (act >>= writeIORef result . Just)
  t0  <- getCurrentTime
  let loop = do
        stat <- threadStatus tid
        case stat of
          ThreadFinished  -> readIORef result
          ThreadBlocked r -> timeCheckAndLoop
          ThreadDied      -> do putStrLn " [lvish-tests] Time-out check -- thread died!"
                                return Nothing
          ThreadRunning   -> timeCheckAndLoop
      timeCheckAndLoop = do 
            now <- getCurrentTime
            let delt :: Double
                delt = fromRational$ toRational$ diffUTCTime now t0
            if delt >= interval
              then do putStrLn " [lvish-tests] Time-out: out of time, killing test thread.."
                      killThread tid
                      -- TODO: <- should probably wait for it to show up as dead.
                      return Nothing
              else do threadDelay (10 * 1000) -- Sleep 10ms.
                      loop   
  loop

{-# NOINLINE timeOutPure #-}
-- | Evaluate a pure value to weak-head normal form, with timeout.
--   This is NONDETERMINISTIC, so its type is sketchy:
--
-- WARNING: This doesn't seem to work properly yet!  I am seeing spurious failures.
-- -RRN [2013.10.24]
--
timeOutPure :: Show a => Double -> a -> Maybe a
timeOutPure tm thnk =
  unsafePerformIO (timeOut tm (evaluate thnk))

assertOr :: Assertion -> Assertion -> Assertion
assertOr act1 act2 = 
  catch act1 
        (\(e::SomeException) -> act2)


nTimes :: Int -> (Int -> IO a) -> IO ()
nTimes 0 _ = return ()
nTimes n c = c n >> nTimes (n-1) c

{-# INLINE for_ #-}
-- | Inclusive/Inclusive
for_ :: Monad m => (Int, Int) -> (Int -> m ()) -> m ()
for_ (start, end) fn | start > end = forDown_ (end, start) fn
for_ (start, end) fn = loop start
  where
  loop !i | i > end  = return ()
          | otherwise = do fn i; loop (i+1)


-- | Inclusive/Inclusive, iterate downward.
forDown_ :: Monad m => (Int, Int) -> (Int -> m ()) -> m ()
forDown_ (start, end) _fn | start > end = error "forDown_: start is greater than end"
forDown_ (start, end) fn = loop end
  where
  loop !i | i < start = return ()
          | otherwise = do fn i; loop (i-1)


-- | Split an inclusive range into N chunks.
--   This may return less than the desired number of pieces if there aren't enough
--   elements in the range.
splitRange :: Int -> (Int,Int) -> [(Int,Int)]
splitRange pieces (start,end)
  | len < pieces = [ (i,i) | i <- [start .. end]]
  | otherwise = chunks
 where
    len = end - start + 1 
    chunks = map largepiece [0..remain-1] ++
             map smallpiece [remain..pieces-1]
    (portion, remain) = len `quotRem` pieces
    largepiece i =
        let offset = start + (i * (portion + 1))
        in (offset, (offset + portion))
    smallpiece i =
        let offset = start + (i * portion) + remain
        in (offset, (offset + portion - 1))

-- | Print out a SELFTIMED message reporting the time from a given test.
timeit :: IO a -> IO a 
timeit ioact = do 
   start <- getCurrentTime
   res <- ioact
   end   <- getCurrentTime
   putStrLn$ "SELFTIMED: " ++ show (diffUTCTime end start)
   return res

-- | An alternate version of `defaultMain` which sets the number of test running
--   threads to one by default, unless the user explicitly overrules it with "-j".
defaultMainSeqTests :: [TF.Test] -> IO ()
defaultMainSeqTests tests = do
  putStrLn " [*] Default test harness..."
  args <- getArgs
  x <- interpretArgs args
  res <- try (case x of
             Left err -> error$ "defaultMainSeqTests: "++err
             Right (opts,_) -> do let opts' = ((mempty{ ropt_threads= Just 1
                                                      , ropt_test_options = Just (mempty{ 
                                                          topt_timeout=(Just$ Just defaultTestTimeout)})})
                                               `mappend` opts)
                                  putStrLn $ " [*] Using "++ show (ropt_threads opts')++ " worker threads for testing."
                                  defaultMainWithOpts tests opts'
                               )
  case res of
    Left (e::ExitCode) -> do
       putStrLn$ " [*] test-framework exiting with: "++show e
       performGC
       putStrLn " [*] GC finished on main thread."
       threadDelay (30 * 1000)
       putStrLn " [*] Main thread exiting."
       exitWith e

-- | In nanoseconds.
defaultTestTimeout :: Int
-- defaultTestTimeout = 3*1000*1000
defaultTestTimeout = 10*1000*1000
-- defaultTestTimeout = 100*1000*1000
