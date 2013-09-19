{-# LANGUAGE BangPatterns, CPP #-}

-- | To make it easier to build (multithreaded) tests

module TestHelpers
 ( 
   -- * Testing parameters
   numElems, getNumAgents, producerRatio,

   -- * Utility for controlling the number of threads used by generated tests.
   setTestThreads,

   -- * Test initialization, reading common configs
   stdTestHarness
 )
 where 

import Data.IORef
import Control.Monad
import qualified Data.Set as S
import Text.Printf
import Control.Concurrent (forkOS, forkIO, ThreadId)
-- import Control.Exception (catch, SomeException, fromException, bracket, AsyncException(ThreadKilled))
import Control.Exception (bracket)
import System.Environment (withArgs, getArgs, getEnvironment)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit  (hUnitTestToTests)
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
numElems :: Int
numElems = case lookup "NUMELEMS" theEnv of 
             Nothing  -> 100 * 1000 -- 500000
             Just str -> warnUsing ("NUMELEMS = "++str) $ 
                         read str

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

-- | Repeate a group of tests while varying the number of OS threads used.  Also,
-- read configuration info.
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
                      Nothing -> S.toList$ S.fromList$
                        [1, 2, np `quot` 2, np, 2*np ]
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

