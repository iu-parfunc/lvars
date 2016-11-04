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
import           System.IO.Unsafe
import qualified System.Random.PCG.Fast.Pure as PCG

import           Control.LVish                 hiding (parForTree)
import           Control.LVish.Internal.Unsafe ()
import           Data.LVar.PureSet             (ISet)
import qualified Data.LVar.PureSet             as PS
import           Data.Set                      as S

import Data.VerifiedOrd.Instances

import           Utils (Measured (..))
import qualified Utils as U

type Det = 'Ef 'P 'G 'NF 'B 'NI

type Bench = IO [(Int, Measured)]

{-# NOINLINE fromSize #-}
fromSize :: Int64
fromSize = unsafePerformIO $ read <$> getEnv "FROMSIZE"

{-# NOINLINE toSize #-}
toSize :: Int64
toSize = unsafePerformIO $ read <$> getEnv "TOSIZE"

{-# NOINLINE seed #-}
seed :: Word64
seed = unsafePerformIO $ read <$> getEnv "SEED"

{-# NOINLINE range #-}
range :: Int64
range = unsafePerformIO $ read <$> getEnv "RANGE"

{-# NOINLINE iters #-}
iters :: Int64
iters = unsafePerformIO $ read <$> getEnv "ITERS"

{-# NOINLINE threads #-}
threads :: Int
threads = unsafePerformIO $ read <$> getEnv "THREADS"

-- | This is the same parForTree from lvish, but using Integral
parForTree :: (Integral n, Show n) => (n, n) -> (n -> Par e s ()) -> Par e s ()
parForTree (start, end) _
  | start > end = error $ "parForTree: start is greater than end: " ++ show (start, end)
parForTree (start, end) body = do
  loop 0 (end - start)

  where
    loop offset remain
      | remain == 1 = body offset
      | otherwise = do
          let (half, rem) = remain `quotRem` 2
          fork $ loop offset half
          loop (offset + half) (half + rem)

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
      fs <- U.fold 0 fromSize S.empty (\s i -> S.insert i s) (\_ -> U.rand g range)
      runParPolyIO $ do
        ps <- PS.newSet fs :: Par Det s (ISet s Int64)
        measure $ parForTree (fromSize, toSize) $
          \_ -> do
            k <- liftIO (U.rand g range)
            PS.insert k ps

vPureSetBench :: Bench
vPureSetBench = do
  g <- PCG.restore (PCG.initFrozen seed)
  U.fori 1 threads $
    \t -> do
      setNumCapabilities t
      fs <- U.fold 0 fromSize S.empty (\s i -> S.insert i s) (\_ -> U.rand g range)
      runParPolyIO $ do
        ps <- PS.newSet fs :: Par Det s (ISet s Int64)
        measure $ parForTree (fromSize, toSize) $
          \_ -> do
            k <- liftIO (U.rand g range)
            PS.vinsert vordInt64 k ps

main :: IO ()
main = do
  !pureSetMeasures <- pureSetBench
  !vpureSetMeasures <- vPureSetBench
  createDirectoryIfMissing True "tests/verified/reports"
  LBS.writeFile "tests/verified/reports/pureset.csv" . encode $
    (\(t, m) -> (t, measTime m)) `fmap` pureSetMeasures
  LBS.writeFile "tests/verified/reports/vpureset.csv" . encode $
    (\(t, m) -> (t, measTime m)) `fmap` vpureSetMeasures
