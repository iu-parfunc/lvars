{-# LANGUAGE RankNTypes, BangPatterns, DataKinds #-}

import Control.LVish hiding (for_)
-- import Control.LVish.DeepFrz (NonFrzn)
import Control.Par.EffectSigs
-- import Data.LVar.IVar
import qualified Data.LVar.PureSet as PS
import qualified Data.LVar.SLSet   as SS

import qualified Data.LVar.PureMap as PM
import qualified Data.LVar.SLMap   as SM
    
import Criterion.Main
import Criterion.Types
import GHC.Conc(numCapabilities)
    
main :: IO ()
main = defaultMain [
  bgroup "basics"
    [ bench "NOOP" $ benchPar (return ())
    , bench "fork" $ benchPar (fork (return ()))
    , bench "new"  $ benchPar  (do _ <- new; return ())
--    , bench "newFull"     $ benchPar (do iv <- newFull (3::Int); return ())
--    , bench "newFull-get" $ benchPar (do iv <- newFull (3::Int); get iv; return ())
    , bench "new-put"      $ benchPar (do iv <- new; put iv (3::Int); return ())
    , bench "new-put-get"  $ benchPar (do iv <- new; put iv (3::Int);        _ <- get iv; return ())
    , bench "new-fork-get" $ benchPar (do iv <- new; fork (put iv (3::Int)); _ <- get iv; return ())
    ],
    
  bgroup "pureset" (basicContainerBench PS.newEmptySet PS.insert PS.newFromList),
  bgroup "slset"   (basicContainerBench SS.newEmptySet SS.insert SS.newFromList),
  bgroup "puremap" (basicContainerBench PM.newEmptyMap (\n -> PM.insert n n) (PM.newFromList . (map (\n->(n,n))))),
  bgroup "slmap"   (basicContainerBench SM.newEmptyMap (\n -> SM.insert n n) (SM.newFromList . (map (\n->(n,n)))))
  ]

-- type Det = Ef P G NF B NI
type Full = Ef P G F B I 
    
-- basicContainerBench :: (Par Det NonFrzn c)
--                     -> (Int -> c -> Par Det NonFrzn ())
basicContainerBench :: (forall s . Par Full s (c s Int))
                    -> (forall s . Int -> c s Int -> Par Full s ())
                    -> (forall s . [Int] -> Par Full s (c s Int))
                    -> [Benchmark]
basicContainerBench mknew insert frmList = 
    [ bench "new"    $ benchPar (new >> return ())
    , bench "insert" $ benchPar (do s <- mknew; insert (99::Int) s)
    , bench "fromList10" $ benchPar (do _ <- frmList [1..10::Int]; return ())
    , bench "fill10"  $ benchPar (do s <- mknew; for_ 1 10  (\i -> insert i s))
    , bench "fill100" $ benchPar (do s <- mknew; for_ 1 100 (\i -> insert i s))
--    , bench "fillWait10" $ benchPar (do s <- PS.newEmptySet; for_ 1 10 (\i -> PS.insert i s))

    -- Here we fill a larger and larger set.  This is mainly useful to observe how linear the plot is:
    , bench "fillN" $ Benchmarkable (\num -> runParNonDet$
                                     do s <- mknew; for_ 1 (fromIntegral num) (\i -> insert i s))
    ]
{-# INLINE basicContainerBench #-}
  
----------------------------------------------------------------------------------------------------
  
-- | Helper for benchmarking par-monad actions:
benchPar :: (forall s . Par Full s ()) -> Benchmarkable
benchPar par = Benchmarkable $ \ iters -> 
  runParNonDet $ rep (fromIntegral iters) par
{-# INLINE benchPar #-}

-- | My own forM for inclusive numeric ranges (not requiring deforestation optimizations).
for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ start end _fn | start > end = error "for_: start is greater than end"
for_ start end fn = loop start
  where
   loop !i | i > end  = return ()
	   | otherwise = do fn i; loop (i+1)
{-# INLINE for_ #-}

rep :: Monad m => Int -> (m ()) -> m ()
rep n m = for_ 1 n (\_ -> m)
{-# INLINE rep #-}


