{-# LANGUAGE RankNTypes, BangPatterns, DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}

import Control.LVish hiding (for_)
import Control.LVish.DeepFrz (runParThenFreeze, NonFrzn)
import Control.Par.EffectSigs
-- import Data.LVar.IVar
import qualified Data.LVar.PureSet as PS
import qualified Data.LVar.SLSet   as SS
import qualified Data.Set          as Set
import qualified Data.Map          as M
    
import qualified Data.LVar.PureMap as PM
import qualified Data.LVar.SLMap   as SM

import qualified Data.LVar.SatMap  as Sat
import qualified Data.LVar.LayeredSatMap as LSM
    
import Criterion.Main
import Criterion.Types
import GHC.Conc(numCapabilities)

-- data IntPar = forall s . IntPar (Par Det s Int)
data IntPar = IntPar (forall s . Par Det s Int)
    
whnf2 :: ((forall s . Par Det s Int) -> b) -> (forall s . Par Det s Int) -> Benchmarkable
whnf2 = whnf  -- Eta expansion does not work here!!

whnf' :: (IntPar -> b) -> IntPar -> Benchmarkable
whnf' = whnf

runIntPar :: IntPar -> Int
runIntPar (IntPar p) = runPar p
        
main :: IO ()
main = defaultMain $
       -- insideOut  -- OPTIONALLY do this... it can make them easier to view in the report.
 [
  bgroup "basics"
    [
      bench "runPar" $ whnf' runIntPar (IntPar $ return 3)
    , bench "runParThenFreeze" $ whnf runParThenFreeze (return (3::Int))
    , bench "NOOP" $ benchPar (return ())
    , bench "fork" $ benchPar (fork (return ()))
    , bench "new"  $ benchPar  (do _ <- new; return ())
--    , bench "newFull"     $ benchPar (do iv <- newFull (3::Int); return ())
--    , bench "newFull-get" $ benchPar (do iv <- newFull (3::Int); get iv; return ())
    , bench "new-put"      $ benchPar (do iv <- new; put iv (3::Int); return ())
    , bench "new-put-get"  $ benchPar (do iv <- new; put iv (3::Int);        _ <- get iv; return ())
    , bench "new-fork-get" $ benchPar (do iv <- new; fork (put iv (3::Int)); _ <- get iv; return ())
    ],

  bgroup "baseline"
     [ bench "Data.Set.fromList" $ nf Set.fromList [1..(10::Int)]
     ],

  bgroup "pureset" (copyContainerBench PS.newEmptySet PS.insert PS.newFromList (PS.copy)),
  bgroup "slset"   (copyContainerBench SS.newEmptySet SS.insert SS.newFromList (SS.copy)),
  bgroup "puremap" (copyContainerBench PM.newEmptyMap (\n -> PM.insert n n) (PM.newFromList . pairit) (PM.copy)),
  bgroup "slmap"   (copyContainerBench SM.newEmptyMap (\n -> SM.insert n n) (SM.newFromList . pairit) (SM.copy)),

  bgroup "satmap"   (basicContainerBench Sat.newEmptyMap (\n -> Sat.insert n n) (Sat.newFromList . pairit)),
  bgroup "layermap" (copyContainerBench LSM.newEmptyMap (\n -> LSM.insert n n) (LSM.newFromList . pairit) LSM.pushLayer)
  ]
 where
   pairit = map (\n->(n,n))

-- Flip a list of BenchGroup's of Benchmarks so the inner benchmark
-- names become the groups.
insideOut :: [Benchmark] -> [Benchmark]
insideOut allbs = [ BenchGroup k bs
                  | (k,bs) <- M.toList mp ]
 where
   mp = M.fromListWith (++)
        [ (n2,[Benchmark n1 b])
        | BenchGroup n1 ls <- allbs
        , Benchmark n2 b <- ls ]
            
type Det = Ef P G NF B NI
type Full = Ef P G F B I 
    
basicContainerBench :: (forall s . Par Full s (c s Int))
                    -> (forall s . Int -> c s Int -> Par Full s ())
                    -> (forall s . [Int] -> Par Full s (c s Int))
                    -> [Benchmark]
basicContainerBench mknew insert frmList =
    [ bench "new"    $ benchPar (new >> return ())
    , bench "insert" $ benchPar (do s <- mknew; insert (99::Int) s)
    -- This doesn't work yet... the list->set connversion gets optimized away.
    , bench "fromList10" $ benchPar (do _ <- frmList [1..x::Int]; return ())
    , bench "fill10"  $ benchPar (do s <- mknew; for_ 1 10  (\i -> insert i s))
    , bench "fill100" $ benchPar (do s <- mknew; for_ 1 100 (\i -> insert i s))
--    , bench "fillWait10" $ benchPar (do s <- PS.newEmptySet; for_ 1 10 (\i -> PS.insert i s))

    -- Here we fill a larger and larger set.  This is mainly useful to observe how linear the plot is:
    , bench "fillN" $ Benchmarkable (\num -> runParNonDet$
                                     do s <- mknew; for_ 1 (fromIntegral num) (\i -> insert i s))
    ]
{-# INLINE basicContainerBench #-}


{-# NOINLINE x #-}
x :: Int
x = 10

copyContainerBench :: (forall s . Par Full s (c s Int))
                    -> (forall s . Int -> c s Int -> Par Full s ())
                    -> (forall s . [Int] -> Par Full s (c s Int))
                    -> (forall s. c s Int -> Par Full s (c s Int))
                    -> [Benchmark]
copyContainerBench mknew insert frmList copy =
  basicContainerBench mknew insert frmList ++ 
   [ bench "fillCopy10x10" $ benchPar (do let copyLoop 0 _ = return ()
                                              copyLoop n s = do s' <- copy s
                                                                for_ 1 10 (\i -> insert (n*100+i) s')
                                                                copyLoop (n-1) s'
                                          s <- mknew;
                                          copyLoop 10 s) ]
{-# INLINE copyContainerBench #-}
  
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

