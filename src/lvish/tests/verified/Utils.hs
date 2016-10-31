{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Utils where

import           Control.Concurrent
import           Control.DeepSeq
import qualified Control.Exception           as E
import           Control.Monad
import           Control.Monad.Trans
import           Data.Int
import           Data.List                   (sort)
import           GHC.Generics
import           GHC.Stats                   (GCStats (..))
import qualified GHC.Stats                   as Stats
import           GHC.Word
import qualified System.Clock                as C
import           System.CPUTime.Rdtsc
import           System.IO
import           System.Mem
import qualified System.Random.PCG.Fast.Pure as PCG

{-# INLINE rand #-}
rand :: PCG.GenIO -> Int64 -> IO Int64
rand !g !n = PCG.uniformR (0, n) g

-- | Run a function several times and take the median time.
{-# INLINE run #-}
run :: Int -> IO Measured -> IO Measured
run runs fn = do
  let rs = if even runs
             then runs + 1
             else runs
      mid = (1 + rs) `quot` 2
  ms <- for 1 rs $ const fn
  return $! sort ms !! (mid - 1)

data Measured =
       Measured
         { measTime               :: !Double
         , measCpuTime            :: !Double
         , measCycles             :: !Int64
         , measIters              :: !Int64
         , measAllocated          :: !Int64
         , measNumGcs             :: !Int64
         , measBytesCopied        :: !Int64
         , measMutatorWallSeconds :: !Double
         , measMutatorCpuSeconds  :: !Double
         , measGcWallSeconds      :: !Double
         , measGcCpuSeconds       :: !Double
         }
  deriving (Eq, Ord, Read, Show, Generic, NFData)

measured :: Measured
measured = Measured
  { measTime = 0
  , measCpuTime = 0
  , measCycles = 0
  , measIters = 0
  , measAllocated = minBound
  , measNumGcs = minBound
  , measBytesCopied = minBound
  , measMutatorWallSeconds = bad
  , measMutatorCpuSeconds = bad
  , measGcWallSeconds = bad
  , measGcCpuSeconds = bad
  }
  where
    bad = -1 / 0

getTime :: IO Double
getTime = ((1.0e-9 *) . fromInteger . C.toNanoSecs) `fmap` C.getTime C.Monotonic

timeit :: String -> IO a -> IO a
timeit msg act = do
  st <- getTime
  x <- act
  en <- getTime
  when (msg /= "") $ do
    putStrLn $ msg ++ ", time: " ++ show (en - st)
    hFlush stdout
  return x

getCPUTime :: IO Double
getCPUTime = ((1.0e-9 *) . fromInteger . C.toNanoSecs) `fmap` C.getTime C.ProcessCPUTime

getCycles :: IO Word64
getCycles = rdtsc

getGCStats :: IO (Maybe GCStats)
getGCStats = (Just `fmap` Stats.getGCStats) `E.catch` \(_ :: E.SomeException) -> return Nothing

applyGCStats :: Maybe GCStats
             -> Maybe GCStats
             -> Measured
             -> Measured
applyGCStats (Just end) (Just start) m = m
  { measAllocated = diff bytesAllocated
  , measNumGcs = diff numGcs
  , measBytesCopied = diff bytesCopied
  , measMutatorWallSeconds = diff mutatorWallSeconds
  , measMutatorCpuSeconds = diff mutatorCpuSeconds
  , measGcWallSeconds = diff gcWallSeconds
  , measGcCpuSeconds = diff gcCpuSeconds
  }
  where
    diff f = f end - f start
applyGCStats _ _ m = m

-- | measure `iters` times
{-# INLINE measure #-}
measure :: (MonadIO m, NFData a) => Int64 -> IO a -> m Measured
measure !iters !f = liftIO $ do
  performGC
  startStats <- getGCStats
  startTime <- getTime
  startCpuTime <- getCPUTime
  startCycles <- getCycles
  _ <- for_ 1 iters $ const f
  endTime <- getTime
  endCpuTime <- getCPUTime
  endCycles <- getCycles
  endStats <- getGCStats
  let !m = applyGCStats endStats startStats $ measured
        { measTime = max 0 (endTime - startTime)
        , measCpuTime = max 0 (endCpuTime - startCpuTime)
        , measCycles = max 0 (fromIntegral (endCycles - startCycles))
        , measIters = iters
        }
  return m

-- | measure once
{-# INLINE measureOnce #-}
measureOnce :: (MonadIO m, NFData a) => IO a -> m Measured
measureOnce = measure 1

{-# INLINE rescale #-}
rescale :: Measured -> Measured
rescale m@Measured { .. } = m
  { measTime = d measTime
  , measCpuTime = d measCpuTime
  , measCycles = i measCycles
  -- skip measIters
  , measNumGcs = i measNumGcs
  , measBytesCopied = i measBytesCopied
  , measMutatorWallSeconds = d measMutatorWallSeconds
  , measMutatorCpuSeconds = d measMutatorCpuSeconds
  , measGcWallSeconds = d measGcWallSeconds
  , measGcCpuSeconds = d measGcCpuSeconds
  }
  where
    d k = maybe k (/ iters) (fromDouble k)
    i k = maybe k (round . (/ iters)) (fromIntegral <$> fromInt k)
    iters = fromIntegral measIters :: Double
    fromDouble d
      | isInfinite d || isNaN d = Nothing
      | otherwise = Just d
    fromInt i
      | i == minBound = Nothing
      | otherwise = Just i

{-# INLINABLE forkJoin #-}
forkJoin :: Int -> (Int -> IO a) -> IO [a]
forkJoin num act = loop num []
  where
    loop 0 !ls = mapM takeMVar ls
    loop n !ls = do
      mv <- newEmptyMVar
      _ <- forkOn (n - 1) $ do
             !v <- act (n - 1)
             putMVar mv v
      loop (n - 1) (mv : ls)

{-# INLINABLE for_ #-}
for_ :: (Num n, Ord n, MonadIO m, NFData a) => n -> n -> (n -> m a) -> m ()
for_ start end _
  | start > end = error "start greater than end"
for_ start end fn = loop start
  where
    loop !i
      | i > end = return ()
      | otherwise = fn i >>= liftIO . E.evaluate . rnf >> loop (i + 1)

for :: (Num n, Ord n, Monad m, NFData a) => n -> n -> (n -> m a) -> m [a]
for start end _
  | start > end = error "start greater than end"
for start end fn = loop start
  where
    loop !i
      | i > end = return []
      | otherwise = do
          !x <- fn i
          !xs <- loop (i + 1)
          return $! x : xs

{-# INLINABLE fori #-}
fori :: (Num n, Ord n, MonadIO m, NFData n, NFData a) => n -> n -> (n -> m a) -> m [(n, a)]
fori start end _
  | start > end = error "start greater than end"
fori start end fn = loop start
  where
    loop !i
      | i > end = return []
      | otherwise = do
          !x <- fn i
          !xs <- loop (i + 1)
          return $! (i, x) : xs

{-# INLINABLE fori' #-}
fori' :: (Num n, Ord n, MonadIO m, NFData n, NFData a) => n -> n -> n -> (n -> m a) -> m [(n, a)]
fori' start end _ _
  | start > end = error "start greater than end"
fori' start end step fn = loop start
  where
    loop !i
      | i > end = return []
      | otherwise = do
          !x <- fn i
          !xs <- loop (i + step)
          return $! (i, x) : xs
