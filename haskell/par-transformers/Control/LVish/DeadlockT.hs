{-# LANGUAGE Unsafe #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, InstanceSigs #-}
{-# LANGUAGE CPP #-}

-- | A module for adding the deadlock-detection capability.


module Control.LVish.DeadlockT
       {-(
         -- * The transformer that adds thecancellation capability
         DeadlockT(), ThreadId,
         
         -- * Operations specific to DeadlockT
         runDeadlockT,
         forkCancelable, createTid, forkCancelableWithTid,
         cancel,         
         pollForCancel,
         cancelMe,

         -- * Boolean operations with cancellation
         asyncAnd
       )-}
       where

import Control.Monad.State as S
import Data.IORef

import Control.Par.Class as PC
import Control.Par.Class.Unsafe (PrivateMonadIO(..))

-- import qualified Data.Atomics.Counter as C

import Data.Atomics.Counter.Unboxed as C

--------------------------------------------------------------------------------

-- | A Par-monad scheduler transformer that adds the deadlock-detection capability.
-- To do this, it must track counts of active (not blocked) computations.
newtype DeadlockT m a = DeadlockT ((StateT DState m) a)
  deriving (Monad, Functor)

unDeadlockT :: DeadlockT t t1 -> StateT DState t t1
unDeadlockT (DeadlockT m) = m

-- The state for each thread is a (hopefully scalable) Counter.
type DState = C.AtomicCounter
-- TODO/FUTURE: For now we disallow nested deadlock tracking... each thread only
-- needs one counter.  We could change this.

instance MonadTrans DeadlockT where
  lift m = DeadlockT (lift m)

-- TODO/FIXME: Replace PrivateMonadIO with something the user can't safely access.
instance PrivateMonadIO m => PrivateMonadIO (DeadlockT m) where
  internalLiftIO m = DeadlockT (lift (internalLiftIO m))

instance PrivateMonadIO m => PrivateMonadIO (StateT s m) where
  internalLiftIO io = lift (internalLiftIO io)
-- data CPair = CPair !Bool ![DState]

--------------------------------------------------------------------------------

-- | Run a Par monad with the deadlock-detection effect.  Return ONLY when all
-- subcomputations have quiesced or blocked.
runDeadlockT :: (ParIVar m, PrivateMonadIO m) => DeadlockT m a -> m a
runDeadlockT (DeadlockT task) = do
  ref <- internalLiftIO $ C.newCounter 1
  let task' = do _ <- task
                 internalLiftIO (C.incrCounter (-1) ref)
                 return ()
  PC.fork (evalStateT task' ref)
  -- TODO: replace the decrement with a zero-check followed by some signaling...
  -- TODO: The main thread should wait on the signal...
  return undefined

instance (ParSealed m) => ParSealed (DeadlockT m) where
  type GetSession (DeadlockT m) = GetSession m

instance (PrivateMonadIO m, ParIVar m, LVarSched m) => LVarSched (DeadlockT m) where
  type LVar (DeadlockT m) = LVar m 
  forkLV act = PC.fork act    
  newLV act = lift$ newLV act  
  stateLV lvar = let (_::Proxy (m ()), a) = stateLV lvar
                 in (Proxy::Proxy((DeadlockT m) ()), a)
  putLV lv putter = lift $ putLV lv putter    
  getLV lv globThresh deltThresh = do
     x <- lift $ getLV lv globThresh deltThresh
     return x
  returnToSched = lift returnToSched

instance (ParQuasi m qm) => ParQuasi (DeadlockT m) (DeadlockT qm) where
  toQPar :: (DeadlockT m) a -> (DeadlockT qm) a 
  toQPar (DeadlockT (S.StateT{runStateT})) =
    DeadlockT $ S.StateT $ toQPar . runStateT

instance (Functor qm, Monad qm, PrivateMonadIO m, ParIVar m, 
          LVarSched m, LVarSchedQ m qm, ParQuasi (DeadlockT m) (DeadlockT qm) ) =>
         LVarSchedQ (DeadlockT m) (DeadlockT qm) where

  freezeLV :: forall a d . LVar (DeadlockT m) a d -> (Proxy ((DeadlockT m) ()), (DeadlockT qm) ())
  freezeLV lvar = (Proxy, (do
    let lvar2 :: LVar m a d
        lvar2 = lvar -- This works because of the specific def for "type LVar" in the instance above...
    let frz :: LVar m a d -> (Proxy (m()), qm ())
        frz x = freezeLV x 
    DeadlockT (lift (snd (frz lvar2)))
    return ()))

-- TODO: Once these classes are refactored, don't provide Future or IVar...
instance ParFuture m => ParFuture (DeadlockT m) where
  type Future (DeadlockT m) = Future m
  type FutContents (DeadlockT m) a = PC.FutContents m a
#if 0  
  spawn_ (DeadlockT task) = DeadlockT $ do
     s0 <- S.get
     lift $ PC.spawn_ $ do
           -- This spawned computation is part of the same tree for purposes of
           -- cancellation:
           (res,_) <- S.runStateT task s0
           return res     
  get iv = DeadlockT $ lift $ PC.get iv
#endif

instance (PrivateMonadIO m, PC.ParIVar m) => PC.ParIVar (DeadlockT m) where
  fork (DeadlockT task) = DeadlockT $ do
    s0 <- S.get
    lift $ internalLiftIO$ C.incrCounter 1 s0
    let task' = do x <- task
                   internalLiftIO (C.incrCounter (-1) s0)
                   return x
    lift $ PC.fork $ do  
      (res,_) <- S.runStateT task'  s0
      return res
#if 0      
  new       = DeadlockT$ lift PC.new
  put_ iv v = DeadlockT$ lift$ PC.put_ iv v
#endif  

--------------------------------------------------------------------------------
-- TEMPORARY: specialized get operations that decrement the counter before blocking...
--------------------------------------------------------------------------------

-- getIV :: IVar 
getIV = undefined
