

import HSBencher
import HSBencher.Methods

import qualified Data.Map as M
import System.Environment (getEnvironment)
import System.Directory   (setCurrentDirectory, getDirectoryContents, getCurrentDirectory)
import System.IO.Unsafe   (unsafePerformIO)
import GHC.Conc           (getNumProcessors)

--------------------------------------------------------------------------------

benches = 
  [ Benchmark "cfa/0CFA_lvish.hs" (words "-t fvExample") withthreads  
  ]
  ++
  [ Benchmark "graphs/bfs_lvish.hs" (words args) withthreads
  | args <- [ "7 100000"
            , "8 100000" ]
  ]

--------------------------------------------------------------------------------

main = do
  putStrLn$ "Automatic thread selection: "++show threadSelection
  defaultMainWithBechmarks benches

withthreads = defaultHSSettings$ varyThreads none
none = And []

-- | Baseline options for GHC.
defaultHSSettings spc =
  And [
        Set NoMeaning (CompileParam "-O2 -threaded -rtsopts")
      , Set NoMeaning (RuntimeParam "+RTS -s -qa -RTS")
--      , Set NoMeaning (CmdPath      "ghc" "ghc") -- Does nothing.
      , spc]

-- | GHC specific method of varying threads.
varyThreads :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
varyThreads conf = And [ conf, Or (map fn threadSelection) ]
 where
   fn n = Set (Threads n) $ RuntimeParam ("+RTS -N"++ show n++" -RTS")

threadSelection :: [Int]
threadSelection = unsafePerformIO $ do
  p   <- getNumProcessors
  return [1 .. p]
  

--------------------------------------------------------------------------------

-- envExample =
--   Or [ And [ Set NoMeaning   (CompileParam "-DNOTHREADING")
--            , Set (Threads 1) (RuntimeEnv "CILK_NPROCS" "1") ]
--      , And [ Set NoMeaning   (CompileParam "-DTHREADING")
--            , Or [ Set (Threads 3) (RuntimeEnv "CILK_NPROCS" "3")
--                 , Set (Threads 4) (RuntimeEnv "CILK_NPROCS" "4")
--                 ]
--            ]
--      ]

Just threader = setThreads ghcMethod 
