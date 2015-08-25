{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- |
module Data.LVar.Future (Future) where

import           Control.Concurrent.MVar
import qualified Control.LVish.Basics                   as LV
import           Control.LVish.Internal                 (Par (WrapPar))
import           Control.Par.EffectSigs
import qualified Internal.Control.LVish.SchedIdempotent as LI

import qualified Control.Par.Class                      as PC
import qualified Control.Par.Class.Unsafe               as PU

--------------------------------------------------------------------------------

newtype Future s a = Future (MVar a)

spawn_ :: forall e s a . Par e s a -> Par e s (Future s a)
spawn_ p = PU.internalCastEffects comp
  where
    comp :: LV.Par ('Ef 'P 'G 'F 'B 'I) s (Future s a)
    comp = do
      r <- PU.internalLiftIO newEmptyMVar
      LV.fork (PU.internalCastEffects p >>= PU.internalLiftIO . putMVar r)
      return (Future r)

instance PC.ParFuture Par where
  type Future Par = Future

  spawn_ = spawn_

  read (Future f) =
    WrapPar $ LI.Par $ \k -> LI.ClosedPar $ \s -> readMVar f >>= \f' -> LI.exec (k f') s
