{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP, DataKinds, GeneralizedNewtypeDeriving, InstanceSigs,
             MultiParamTypeClasses, NamedFieldPuns, ScopedTypeVariables,
             TypeFamilies, UndecidableInstances #-}

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
import Control.Par.Class.Unsafe as PC
import Control.Par.EffectSigs

import qualified Data.Atomics.Counter as C

--------------------------------------------------------------------------------


-- | A Par-monad scheduler transformer that adds the deadlock-detection capability.
-- To do this, it must track counts of active (not blocked) computations.
newtype DeadlockT (p :: EffectSig -> * -> * -> *) e s a =
    DeadlockT { unDeadlockT :: StateT DState (p e s) a }

-- The state for each thread is a (hopefully scalable) Counter.
type DState = C.AtomicCounter

--------------------------------------------------------------------------------
-- | Run a Par monad with the deadlock-detection effect.  Return ONLY when all
-- subcomputations have quiesced or blocked.
runDeadlockT :: (ParIVar p, GetG e ~ NG) => DeadlockT p e s a -> p e s a
runDeadlockT (DeadlockT task) = do
   ref <- internalLiftIO $ C.newCounter 1
   let
     task' = do
       undefined
       -- _ <- task
       -- internalLiftIO (C.incrCounter (-1) ref)
       -- return ()
   PC.fork (evalStateT task' ref)
   -- TODO: replace the decrement with a zero-check followed by some signaling...
   -- TODO: The main thread should wait on the signal...
   return undefined


-- TODO/FUTURE: For now we disallow nested deadlock tracking... each thread only
-- needs one counter.  We could change this.

instance ParMonad p => ParMonad (DeadlockT p) where
  internalLiftIO io = DeadlockT $ S.lift (internalLiftIO io)
  pbind (DeadlockT pa) f = DeadlockT $ pa >>=
                           (\x -> let DeadlockT pb = f x
                                  in pb)                                        
  preturn x = DeadlockT $ return x

  -- Every fork increments the number of live tasks by one:
  fork (DeadlockT task) = DeadlockT $ do
    s0 <- S.get
    S.lift $ internalLiftIO$ C.incrCounter 1 s0
    let task' = do x <- task 
                   S.lift$ internalLiftIO (C.incrCounter (-1) s0)
                   return x
    S.lift $ PC.fork $ do
      (res,_) <- S.runStateT task'  s0
      return res

liftDT :: ParMonad p => p e s a -> DeadlockT p e s a
liftDT pa = DeadlockT (S.lift pa)

instance (ParMonad m, ParIVar m, LVarSched m) => LVarSched (DeadlockT m) where
  type LVar (DeadlockT m) = LVar m
  -- This fork calls the ParMonad.fork above, which increments the counter:
  forkLV act = PC.fork act
  newLV act = liftDT$ newLV act

  stateLV :: forall e s a d . (LVar (DeadlockT m) a d)
             -> (Proxy ((DeadlockT m) e s ()), a)
  stateLV lvar = let prx :: Proxy (m e s ())
                     (prx, a) = stateLV lvar
                 in (Proxy::Proxy((DeadlockT m e s) ()), a)
  putLV lv putter = liftDT $ putLV lv putter

  -- Gets may block, and must decrement the counter if they do:
  getLV lv globThresh deltThresh = do
     x <- liftDT $ getLV lv gThresh' dThresh'
     return x
   where
     -- TODO: Some delicate guarantees need to be established here:
     --  For each GET call:
     --    * globalThresh is called exactly once
     --    * if globalThresh returns Nothing, then dThresh is called AT LEAST once
     --    * get only returns when dThresh returns Just
     --    * dzThresh is never called again after it returns Just.
     
     -- TODO: figure out where to mess with the counter here:
     gThresh' st frzn = do x <- globThresh st frzn
                           case x of
                            Just a -> return x 
                            Nothing -> error "FINISHME" -- Decr counter?
     dThresh' delt = do x <- deltThresh delt
                        case x of
                         Just a -> error "FINISHM" -- INCR here, assume this happens ONCE
                         Nothing -> return Nothing

  returnToSched = liftDT returnToSched
{-  
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
#if 0
  new       = DeadlockT$ lift PC.new
  put_ iv v = DeadlockT$ lift$ PC.put_ iv v
#endif

--------------------------------------------------------------------------------
-- TEMPORARY: specialized get operations that decrement the counter before blocking...
--------------------------------------------------------------------------------

-- getIV :: IVar
getIV = undefined

-}
