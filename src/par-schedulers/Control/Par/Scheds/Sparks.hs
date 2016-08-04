{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

module Control.Par.Scheds.Sparks
  ( Par
  , runPar
  , runParPoly
  ) where

import           Control.Monad                   (void)
import           System.IO.Unsafe                (unsafePerformIO)

import           Control.Par.Class
import qualified Control.Par.Class.Unsafe        as PC
import           Control.Par.EffectSigs

import qualified Control.Monad.Par.Scheds.Sparks as S

newtype Par (e :: EffectSig) s a =
  Sparks { unwrapSparks :: S.Par a }

newtype SparksFuture s a = SparksFuture { unwrapSparksFuture :: S.Future a }

instance PC.ParMonad Par where
  pbind (Sparks p1) p2 = Sparks $ p1 >>= unwrapSparks . p2
  preturn = Sparks . return
  fork = void . Sparks . S.spawn_ . unwrapSparks
  internalLiftIO = return . unsafePerformIO

instance ParFuture Par where
  type Future Par = SparksFuture
  type FutContents Par a = ()
  spawn_ = Sparks . fmap SparksFuture . S.spawn_ . unwrapSparks
  get = Sparks . S.get . unwrapSparksFuture

runPar :: (forall s. Par ('Ef 'P 'G 'NF 'B 'NI) s a) -> a
runPar (Sparks p) = S.runPar p

runParPoly :: Deterministic e => Par e s a -> a
runParPoly (Sparks p) = S.runPar p
