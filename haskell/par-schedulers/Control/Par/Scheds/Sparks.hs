{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Control.Par.Scheds.Sparks where

import           Control.Monad                   (void)
import           System.IO.Unsafe                (unsafePerformIO)

import           Control.Par.Class
import qualified Control.Par.Class.Unsafe        as PC
import           Control.Par.EffectSigs

import qualified Control.Monad.Par.Scheds.Sparks as S

newtype Sparks (e :: EffectSig) s a =
  Sparks { unwrapSparks :: S.Par a }

newtype SparksFuture s a = SparksFuture { unwrapSparksFuture :: S.Future a }

instance PC.ParMonad Sparks where
  pbind (Sparks p1) p2 = Sparks $ p1 >>= unwrapSparks . p2
  preturn = Sparks . return
  fork = void . Sparks . S.spawn_ . unwrapSparks
  internalLiftIO = return . unsafePerformIO

instance ParFuture Sparks where
  type Future Sparks = SparksFuture
  spawn_ = Sparks . fmap SparksFuture . S.spawn_ . unwrapSparks
  read   = Sparks . S.get . unwrapSparksFuture
