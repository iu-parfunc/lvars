{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Control.Par.Scheds.Trace where

import           System.IO.Unsafe               (unsafePerformIO)

import           Control.Par.Class
import qualified Control.Par.Class.Unsafe       as PC
import           Control.Par.EffectSigs

import           Control.Monad.Par.Scheds.Trace as T

newtype Trace (e :: EffectSig) s a =
  Trace { unwrapTrace :: Par a }

newtype TraceIVar s a = TraceIVar { unwrapTraceIVar :: T.IVar a }

instance PC.ParMonad Trace where
  pbind (Trace p1) p2 = Trace $ p1 >>= unwrapTrace . p2
  preturn = Trace . return
  fork = Trace . T.fork . unwrapTrace
  internalLiftIO = return . unsafePerformIO

instance ParFuture Trace where
  type Future Trace = TraceIVar
  spawn_ = Trace . fmap TraceIVar . T.spawn_ . unwrapTrace
  read = Trace . T.get . unwrapTraceIVar
