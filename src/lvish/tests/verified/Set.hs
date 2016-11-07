{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE Rank2Types   #-}

module Main where

import           Control.DeepSeq
import           Control.Monad.Trans
import qualified Data.ByteString.Lazy          as LBS
import           Data.Csv
import           Data.Int
import           Data.Word
import           GHC.Conc
import           System.Directory
import           System.Environment
import           System.IO.Unsafe
import qualified System.Random.PCG.Fast.Pure   as PCG

import           Control.LVish                 as LV
import           Control.LVish.Internal.Unsafe ()
import qualified Data.LVar.PureSet             as PS
import           Data.LVar.SLSet               as SL

import           Data.VerifiedOrd.Instances

import           Utils                         (Measured (..))
import qualified Utils                         as U

type Det = 'Ef 'P 'G 'NF 'B 'NI

type Bench = IO [(Int, Measured)]

{-# INLINE readFromEnv #-}
readFromEnv :: (Show a, Read a) => String -> IO a
readFromEnv key = do
  !val <- read <$> getEnv key
  putStrLn (key ++ ": " ++ show val)
  return val

{-# NOINLINE size #-}
size :: Int64
size = unsafePerformIO $ readFromEnv "SIZE"

{-# NOINLINE seed #-}
seed :: Word64
seed = unsafePerformIO $ readFromEnv "SEED"

{-# NOINLINE iters #-}
iters :: Int64
iters = unsafePerformIO $ readFromEnv "ITERS"

{-# NOINLINE threads #-}
threads :: Int
threads = unsafePerformIO $ readFromEnv "THREADS"

{-# INLINE measure #-}
measure :: (MonadIO m, NFData a) => m a -> m Measured
measure f = do
  m <- U.measure iters f
  return (U.rescale m)

data SetImpl m =
       SetImpl
         { newEmptySet :: forall s. Par Det s (m s Int64)
         , insert      :: forall s. Int64 -> m s Int64 -> Par Det s ()
         }

pureSetImpl :: SetImpl PS.ISet
pureSetImpl = SetImpl PS.newEmptySet PS.insert

vpureSetImpl :: SetImpl PS.ISet
vpureSetImpl = SetImpl PS.newEmptySet (PS.vinsert vordInt64)

slSetImpl :: SetImpl SL.ISet
slSetImpl = SetImpl SL.newEmptySet SL.insert

vslSetImpl :: SetImpl SL.ISet
vslSetImpl = SetImpl SL.newEmptySet (SL.vinsert vordInt64)

{-# INLINE mkSetBench #-}
mkSetBench :: SetImpl m -> Bench
mkSetBench (SetImpl setNew setIns) = do
  !g <- PCG.restore (PCG.initFrozen seed)
  U.fori 1 threads $
    \t -> do
      setNumCapabilities t
      measure $ runParPolyIO $ do
        !ps <- setNew
        U.for_ 1 size $
          \_ -> fork $ do
            !k <- liftIO (U.rand g)
            setIns k ps

pureSetBench :: Bench
pureSetBench = mkSetBench pureSetImpl

vPureSetBench :: Bench
vPureSetBench = mkSetBench vpureSetImpl

slSetBench :: Bench
slSetBench = mkSetBench slSetImpl

vslSetBench :: Bench
vslSetBench = mkSetBench vslSetImpl

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
