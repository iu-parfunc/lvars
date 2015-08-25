{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

module Control.Par.Scheds.Direct
  ( Par
  , runPar
  , runParPoly
  ) where

import           System.IO.Unsafe                (unsafePerformIO)

import           Control.Par.Class
import qualified Control.Par.Class.Unsafe        as PC
import           Control.Par.EffectSigs

import qualified Control.Monad.Par.Scheds.Direct as D

newtype Par (e :: EffectSig) s a =
  Direct { unwrapDirect :: D.Par a }

newtype DirectIVar s a = DirectIVar { unwrapDirectIVar :: D.IVar a }

instance PC.ParMonad Par where
  pbind (Direct p1) p2 = Direct $ p1 >>= unwrapDirect . p2
  preturn = Direct . return
  fork = Direct . D.fork . unwrapDirect
  internalLiftIO = return . unsafePerformIO

instance ParFuture Par where
  type Future Par = DirectIVar
  type FutContents Par a = ()
  spawn_ = Direct . fmap DirectIVar . D.spawn_ . unwrapDirect
  get = Direct . D.get . unwrapDirectIVar

runPar :: (forall s. Par ('Ef 'P 'G 'NF 'B 'NI) s a) -> a
runPar (Direct p) = D.runPar p

runParPoly :: Deterministic e => Par e s a -> a
runParPoly (Direct p) = D.runPar p
