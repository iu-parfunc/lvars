
{- | The benchmarking script:

USAGE:

   ./run_benchmark [mode] [hsbencher options]

Where mode is 'desktop', 'server', or 'quick'.

-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import qualified Data.Set as S

import GHC.Conc           (getNumProcessors)
import System.Environment (getEnvironment, getArgs, withArgs)
import System.IO.Unsafe   (unsafePerformIO)
import System.Console.GetOpt

import HSBencher.Types(BenchSpace(..), Benchmark(..), ParamSetting(..), DefaultParamMeaning(..),
                       -- compileOptsOnly, enumerateBenchSpace, toCompileFlags,
                       -- makeBuildID, BuildID,
                       mkBenchmark
                      )
import HSBencher.App (defaultMainWithBechmarks, all_cli_options)

--------------------------------------------------------------------------------
-- Main Script
--------------------------------------------------------------------------------

data Mode = Server | Desktop | Quick      deriving (Show,Eq)
data Flag = SetMode Mode
          | SetSched Sched
          | Help
          deriving (Show,Eq)

-- | Supported schedulers:
data Sched 
   = TraceST 
   | DirectST
   | SparksST
   | LVishST
 deriving (Eq, Show, Read, Ord, Enum, Bounded)

options :: [OptDescr Flag]
options =
     [ Option [] ["server"]  (NoArg (SetMode Server))  "server-sized benchmarks"
     , Option [] ["desktop"] (NoArg (SetMode Desktop)) "desktop-sized benchmarks"
     , Option [] ["quick"]   (NoArg (SetMode Quick))   "(default) quick testing"
     , Option ['h'] ["help"] (NoArg Help)              "report this help message"

     , Option [] ["sparks-st"] (NoArg (SetSched SparksST)) "add this scheduler (default is all schedulers)"
     , Option [] ["direct-st"] (NoArg (SetSched DirectST)) "add this scheduler + one transformer"       
     , Option [] ["trace-st"]  (NoArg (SetSched TraceST))  "add this scheduler + one transformer"              
     , Option [] ["lvish-st"]  (NoArg (SetSched LVishST))  "add this scheduler + one transformer"              
     ]

main :: IO ()
main = do
  args <- getArgs
  let (opts,nonopts,unrecog,errs) = getOpt' Permute options args
  -- The first arg is a kind of mode:

  let help1 = usageInfo ("USAGE: run_benchmark [options]\n"++
                        "\nFirst, specific options for this script are:\n")
                options
      help2 = usageInfo (help1++"\nAlso use the generic HSBencher options below:\n")
                        (concat $ map snd all_cli_options)

      activeScheds0 = [ sched | SetSched sched <- opts ]
      activeScheds = if null activeScheds0
                     then defaultSchedSet
                     else S.fromList activeScheds0
  if Help `elem` opts || errs /= [] then
    error help2
   else do
    let passthru = nonopts ++ unrecog
        modes    = [ s | SetMode s <- opts ]
    putStrLn$ "  [Bench script mode selection]: "++ show modes
    putStrLn$ "  [Bench script Sched selection]: "++ show activeScheds
    putStrLn$ "  [Note: passing through options to HSBencher]: "++unwords passthru
    withArgs passthru $ 
     case modes of
       [Desktop] -> defaultMainWithBechmarks (bls_desktop activeScheds)
       [Server]  -> defaultMainWithBechmarks (bls_server activeScheds)
       [Quick]   -> defaultMainWithBechmarks (bls_quick activeScheds)
       []        -> defaultMainWithBechmarks (bls_quick activeScheds)
       ls        -> error$ "Conflicting mode options: "++show ls
    
--------------------------------------------------------------------------------
-- Here are the actual benchmarks:
--------------------------------------------------------------------------------

bls_quick :: S.Set Sched -> [Benchmark DefaultParamMeaning]
bls_quick ss =
 [
--    mergeBench [] ss
 ]

bls_desktop :: S.Set Sched -> [Benchmark DefaultParamMeaning]
bls_desktop ss = 
 [ mergeBench [show sz, show  sthresh, show mthresh, sortmode, mergemode] ss 
 | sz <- [ 25 ]
 , (sortmode,mergemode)  <- [ ("CSort","CMerge"),
                              ("VAMSort","CMerge"),
                              ("VAMSort","MPMerge"),
                              ("VAISort","MPMerge"), 
                              ("CSort","MPMerge")                              
--                              ("VAMSort","TMerge")
                            ]
 , sthresh <- [ 8193 ] -- did 2048 for a while
 , mthresh <- [ 8193 ] -- Bump to 8193.. [2013.11.15]
 ]

bls_server :: S.Set Sched -> [Benchmark DefaultParamMeaning]
bls_server = bls_desktop -- TODO.

mergeBench args@[sz,sthresh,mthresh,sortA,mergeA] ss =
 (mkBenchmark "mergesort/Makefile" args (futures ss))
  { progname=Just$ "mergesort_"++sortA++"_"++mergeA }

-- Factor out boilerplate:
futbench :: String -> [String] -> S.Set Sched -> Benchmark DefaultParamMeaning
futbench dir args ss =
   (mkBenchmark ("src/"++dir++"/generated.cabal")  args  (futures ss)) { progname=Just dir }   

--------------------------------------------------------------------------------
-- Set up some common benchmark config spaces:
--------------------------------------------------------------------------------

-- | Benchmarks that only require futures, not ivars.
futures :: S.Set Sched -> BenchSpace DefaultParamMeaning
futures ss = defaultSettings$ varyThreads $
          Or$ map sched $ S.toList ss

defaultSettings :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
defaultSettings spc =
  And [

--        Set NoMeaning (CompileParam "--disable-library-profiling")
--      , Set NoMeaning (CompileParam "--disable-executable-profiling")
       Set NoMeaning (RuntimeParam "+RTS -s -qa -RTS")
      , spc ]

--------------------------------------------------------------------------------
-- Supporting definitions:
--------------------------------------------------------------------------------

-- | Realize a scheduler selection via a compile flag.
sched :: Sched -> BenchSpace DefaultParamMeaning
sched s =
 --  Set (Variant$ show s) $ CompileParam $ schedToCabalFlag s
--  Set (Variant$ show s) $ RuntimeEnv "WHICHSCHED" (schedToModule s)
    Set (Variant$ show s) $ CompileParam ("-DPARSCHED="++(schedToModule s))

-- | By default, we usually don't test meta-par 
defaultSchedSet :: S.Set Sched
defaultSchedSet = S.fromList [TraceST, SparksST, LVishST] -- Skip Direct.
  -- (S.fromList [minBound ..]) -- All of them.

schedToCabalFlag :: Sched -> String
schedToCabalFlag s =
  case s of
    TraceST  -> "-ftrace-st"
    DirectST -> "-fdirect-st"
    SparksST -> "-fsparks-st"
    LVishST  -> "-flvish-st"

schedToModule :: Sched -> String
schedToModule s =
  case s of
    TraceST  -> "Control.Monad.Par.Scheds.Trace"
    SparksST -> "Control.Monad.Par.Scheds.Sparks"
    DirectST -> "Control.Monad.Par.Scheds.Direct"    
    LVishST  -> "Control.LVish"

-- TODO: make this an option:
threadSelection :: [Int]
threadSelection = unsafePerformIO $ do
  env <- getEnvironment
  p   <- getNumProcessors
  case lookup "THREADS" env of
    Just ls -> return$ map read $ words ls
    -- Arbitrary default policy 
    Nothing
      | p <= 16   -> return  [1 .. p]
      | otherwise -> return$ 1 : [2,4 .. p]

-- | Add variation from thread count.    
varyThreads :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
varyThreads conf = Or
  [
    -- Disabling unthreaded mode:
    -- conf -- Unthreaded mode.
    And [
          -- Set NoMeaning (CompileParam "--ghc-options='-threaded'")
          Or (map fn threadSelection)
        , conf ]
  ]
 where
   fn n = Set (Threads n) $ RuntimeParam  ("+RTS -N"++ show n++" -RTS")
