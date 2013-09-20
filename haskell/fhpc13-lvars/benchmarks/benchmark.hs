
-- | This is an HSBencher script.

module Main where

import qualified Data.Set as Set

import GHC.Conc           (getNumProcessors)
import System.Environment (getEnvironment)
import System.IO.Unsafe   (unsafePerformIO)

import HSBencher.Types(BenchSpace(..), Benchmark(..), ParamSetting(..), DefaultParamMeaning(..)
                       -- compileOptsOnly, enumerateBenchSpace, toCompileFlags,
                       -- makeBuildID, BuildID, 
                      )
import HSBencher.App (defaultMainWithBechmarks)

-- Temp:
-- import Text.PrettyPrint.GenericPretty (Out(doc,docPrec), Generic)

main :: IO ()
main = do
  putStrLn$ "Exploring thread settings: " ++ show threadSelection
  defaultMainWithBechmarks bls

--------------------------------------------------------------------------------
-- Here are the actual benchmarks:
--------------------------------------------------------------------------------

allSettings = varyThreads defaultSettings

stratSet = (And [allSettings,
                 Set NoMeaning (CompileParam "bf-traverse-Strategies")])

monadparSet = (And [allSettings,
                    Set NoMeaning (CompileParam "bf-traverse-monad-par")])

lvarSet = (And [allSettings,
                Set NoMeaning (CompileParam "bf-traverse-monad-par")])

bls :: [Benchmark DefaultParamMeaning]
bls =

  -- # Arguments:
  -- #  - filename of graph
  -- #  - number of hops to take from node 0
  -- #  - microseconds of work to be done by function applied to each node

  ------------------------------------------------------------  
 -- Desktop configuration:
 ------------------------------------------------------------  
 [ Benchmark "name" ["none","/tmp/rand_320000_40000","10","1"] stratSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","2"] stratSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","4"] stratSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","8"] stratSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","16"] stratSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","32"] stratSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","64"] stratSet

 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","1"] monadparSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","2"] monadparSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","4"] monadparSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","8"] monadparSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","16"] monadparSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","32"] monadparSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","64"] monadparSet

 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","1"] lvarSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","2"] lvarSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","4"] lvarSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","8"] lvarSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","16"] lvarSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","32"] lvarSet
 , Benchmark "name" ["none","/tmp/rand_320000_40000","10","64"] lvarSet
 ]

--------------------------------------------------------------------------------
-- Set up some common benchmark config spaces:
--------------------------------------------------------------------------------

-- Add the default Haskell compiler settings that we want:
defaultSettings :: BenchSpace DefaultParamMeaning
defaultSettings =
  And [ Set NoMeaning (CompileParam "--disable-documentation")
      , Set NoMeaning (CompileParam "--disable-library-profiling")
      , Set NoMeaning (CompileParam "--disable-executable-profiling")
      , Set NoMeaning (RuntimeParam "+RTS -s -qa -RTS")
      ]

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
varyThreads conf =
  -- Or [ conf {- Unthreaded mode -}, threaded ]
  threaded
  where
    threaded = And [ Set NoMeaning (CompileParam "--ghc-options='-threaded'")
                   , Or (map fn threadSelection)
                   , conf ]
    fn n = Set (Threads n) $ RuntimeParam  ("+RTS -N"++ show n++" -RTS")

