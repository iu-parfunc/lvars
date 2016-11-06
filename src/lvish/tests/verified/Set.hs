{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE Rank2Types   #-}

module Main where

import           Control.DeepSeq
import           Control.Monad.Trans
import qualified Data.ByteString.Lazy        as LBS
import           Data.Csv
import           Data.Int
import           Data.Word
import           GHC.Conc
import           System.Directory
import           System.Environment
import           System.IO
import           System.IO.Unsafe
import qualified System.Random.PCG.Fast.Pure as PCG

import qualified Control.LVish               as LV
import           Control.LVish.Internal.Unsafe ()
import qualified Data.LVar.PureSet             as PS
import           Data.LVar.SLSet               as SL
import           Data.Set                      as S
import qualified Control.Par.Class     as PC

import Data.VerifiedOrd.Instances

import           Utils (Measured (..))
import qualified Utils as U

type Bench = IO [(Int, Measured)]

{-# NOINLINE sIzE #-}
sIzE :: Int64
sIzE = unsafePerformIO $ read <$> getEnv "SIZE"

{-# NOINLINE seed #-}
seed :: Word64
seed = unsafePerformIO $ read <$> getEnv "SEED"

{-# NOINLINE iters #-}
iters :: Int64
iters = unsafePerformIO $ read <$> getEnv "ITERS"

{-# NOINLINE threads #-}
threads :: Int
threads = unsafePerformIO $ read <$> getEnv "THREADS"

{-# INLINE measure #-}
measure :: (MonadIO m, NFData a) => m a -> m Measured
measure f = do
  m <- U.measure iters f
  return (U.rescale m)

pureSetBench :: Bench
pureSetBench = do
  g <- PCG.restore (PCG.initFrozen seed)
  U.fori 1 threads $
    \t -> do
      setNumCapabilities t
      measure $ LV.runParPolyIO $ LV.isDet $ do
        ps <- PS.newEmptySet
        U.for_ 1 sIzE (\_ -> LV.fork $ do
                          k <- liftIO $ U.rand g
                          PS.insert k ps)

vPureSetBench :: Bench
vPureSetBench = do
  g <- PCG.restore (PCG.initFrozen seed)
  U.fori 1 threads $
    \t -> do
      setNumCapabilities t
      measure $ LV.runParPolyIO $ LV.isDet $ do
        ps <- PS.newEmptySet
        U.for_ 1 sIzE (\_ -> LV.fork $ do
                          k <- liftIO $ U.rand g
                          PS.vinsert vordInt64 k ps)

slSetBench :: Bench
slSetBench = do
  g <- PCG.restore (PCG.initFrozen seed)
  U.fori 1 threads $
    \t -> do
      setNumCapabilities t
      measure $ LV.runParPolyIO $ do
        ps <- SL.newEmptySet
        U.for_ 1 sIzE (\_ -> LV.fork $ do
                          k <- liftIO $ U.rand g
                          SL.insert k ps)

vslSetBench :: Bench
vslSetBench = do
  g <- PCG.restore (PCG.initFrozen seed)
  U.fori 1 threads $
    \t -> do
      setNumCapabilities t
      measure $ LV.runParPolyIO $ do
        ps <- SL.newEmptySet
        U.for_ 1 sIzE (\_ -> LV.fork $ do
                          k <- liftIO $ U.rand g
                          SL.vinsert vordInt64 k ps)

main :: IO ()
main = do
  !pureSetMeasures <- pureSetBench
  !vpureSetMeasures <- vPureSetBench
  !slSetMeasures <- slSetBench
  !vslSetMeasures <- vslSetBench
  createDirectoryIfMissing True "tests/verified/reports"
  LBS.writeFile "tests/verified/reports/pureset.csv" . encode $
    (\(t, m) -> (t, measTime m)) `fmap` pureSetMeasures
  LBS.writeFile "tests/verified/reports/vpureset.csv" . encode $
    (\(t, m) -> (t, measTime m)) `fmap` vpureSetMeasures
  LBS.writeFile "tests/verified/reports/slset.csv" . encode $
    (\(t, m) -> (t, measTime m)) `fmap` slSetMeasures
  LBS.writeFile "tests/verified/reports/vslset.csv" . encode $
    (\(t, m) -> (t, measTime m)) `fmap` vslSetMeasures
