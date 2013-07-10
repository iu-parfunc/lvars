

import HSBencher
import HSBencher.Methods

import qualified Data.Map as M
import System.Environment (getEnvironment)
import System.Directory   (setCurrentDirectory, getDirectoryContents, getCurrentDirectory)
import System.IO.Unsafe   (unsafePerformIO)
import GHC.Conc           (getNumProcessors)

--------------------------------------------------------------------------------

benches = 
  [ Benchmark "cfa/0CFA_lvish.hs" (words args) withthreads
  | args <- [ "-t fvExample"
            ]    
  ]
  ++
  [ Benchmark "graphs/bfs_lvish.hs" (words args) withthreads
  | args <- [
              "bfsI  grid 1000" ++ scale
            , "bfsI  rmat 1000" ++ scale
            , "bfsI  rand 1000" ++ scale
            , "bfsN  grid 1000" ++ scale
            , "bfsN  rmat 1000" ++ scale
            , "bfsN  rand 1000" ++ scale
              
            , "misN3 grid 500" ++ scale 
            , "misI3 grid 2000" ++ scale
            , "misI3 rmat 2000" ++ scale
            , "misI3 rand 2000" ++ scale

            , "bfsN_misI grid 500" ++ scale
            , "bfsN_misI rmat 500" ++ scale
            , "bfsN_misI rand 500" ++ scale
              
            , "bfsN_misI_deg grid 500" ++ scale
            , "bfsN_misI_deg rmat 500" ++ scale
            , "bfsN_misI_deg rand 500" ++ scale              
            ]
  ]
  ++
  [ Benchmark "graphs/bfs_lvish.hs" (words args ) nothreads
  | args <- [ "misSeq grid 2000" ++ scale
            , "bfsS  grid 500" ++ scale
            ]
  ]

-- Multiply by one thousand.  TODO: make changeable.
scale = "000"
-- scale = ""

--------------------------------------------------------------------------------

main = do
  putStrLn$ "Automatic thread selection: "++show threadSelection
  defaultMainModifyConfig $ \ conf ->
    conf{ benchlist  = benches
        , runTimeOut = Just 20 }

nothreads = defaultHSSettings none
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
  return$
    if p <= 4  then [1..p] else
    if p <= 16 then 1: [2,4 .. p]
    else            1:2:[4,8 .. p]

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
