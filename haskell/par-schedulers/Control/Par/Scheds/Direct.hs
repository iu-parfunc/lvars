{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Control.Par.Scheds.Direct where

import           System.IO.Unsafe                (unsafePerformIO)

import           Control.Par.Class
import qualified Control.Par.Class.Unsafe        as PC
import           Control.Par.EffectSigs

import           Control.Monad.Par.Scheds.Direct as D

newtype Direct (e :: EffectSig) s a =
  Direct { unwrapDirect :: Par a }

newtype DirectIVar s a = DirectIVar { unwrapDirectIVar :: D.IVar a }

instance PC.ParMonad Direct where
  pbind (Direct p1) p2 = Direct $ p1 >>= unwrapDirect . p2
  preturn = Direct . return
  fork = Direct . D.fork . unwrapDirect
  internalLiftIO = return . unsafePerformIO

instance ParFuture Direct where
  type Future Direct = DirectIVar
  spawn_ = Direct . fmap DirectIVar . D.spawn_ . unwrapDirect
  read   = Direct . D.get . unwrapDirectIVar
