{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE Rank2Types   #-}

module Main where

import           Control.DeepSeq
import           Control.Monad.Trans
import qualified Data.ByteString.Lazy          as LBS
import           Data.Csv
import           Data.Int
import qualified Data.Map                      as M
import           GHC.Conc
import           System.Directory
import           System.Environment
import           System.IO.Unsafe

import           Control.LVish                 as LV
import           Control.LVish.Internal.Unsafe ()
import           Control.Par.Class
import           Data.LVar.PureMap             ()

import           Sum
import           Utils                         (Measured (..))
import qualified Utils                         as U

import           Language.Haskell.Liquid.ProofCombinators

type Det = 'Ef 'P 'G 'NF 'B 'NI

type Bench = IO [(Int, Measured)]

{-# INLINE readFromEnv #-}
readFromEnv :: (Show a, Read a) => String -> IO a
readFromEnv key = do
  !val <- read <$> getEnv key
  putStrLn (key ++ ": " ++ show val)
  return val

{-# NOINLINE size #-}
size :: Int
size = unsafePerformIO $ readFromEnv "SIZE"

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

{-# INLINE initMap #-}
initMap :: M.Map Int Int
initMap = M.fromList (zip [1 .. size] [1 .. size])

type FoldImpl = M.Map Int Int -> forall s. Par Det s Sum

{-# INLINE pmapFoldImpl #-}
pmapFoldImpl :: FoldImpl
pmapFoldImpl =
  pmapFold (\(_, v) -> return (Sum v)) (\v1 v2 -> return (Sum (getSum v1 + getSum v2))) (Sum 0)

{-# INLINE vpmapFoldImpl #-}
vpmapFoldImpl :: FoldImpl
vpmapFoldImpl =
  vpmapFold (\(_, v) -> return (Sum v)) vMonoidSum

{-# INLINE mkFoldBench #-}
mkFoldBench :: FoldImpl -> Bench
mkFoldBench fld =
  U.fori 1 threads $
    \t -> do
      setNumCapabilities t
      measure $ runParPolyIO (fld initMap)

pmapFoldBench :: Bench
pmapFoldBench = mkFoldBench pmapFoldImpl

vpmapFoldBench :: Bench
vpmapFoldBench = mkFoldBench vpmapFoldImpl

main :: IO ()
main = do
  !pmapFoldMeasures <- pmapFoldBench
  !vpmapFoldMeasures <- vpmapFoldBench
  createDirectoryIfMissing True "tests/verified/reports"
  LBS.writeFile "tests/verified/reports/pmapfold.csv" . encode $
    (\(t, m) -> (t, measTime m)) `fmap` pmapFoldMeasures
  LBS.writeFile "tests/verified/reports/vpmapfold.csv" . encode $
    (\(t, m) -> (t, measTime m)) `fmap` vpmapFoldMeasures
