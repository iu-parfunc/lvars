{-# LANGUAGE Unsafe #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A module for adding the cancellation capability.
-- 
--   In its raw form, this is unsafe, because cancelating could cancel something that
--   would have performed a visible side effect.

module Control.LVish.CancelT
       (
         -- * The transformer that adds thecancellation capability
         CancelT(), ThreadId,
         
         -- * Operations specific to CancelT
         runCancelT,
         forkCancelable,
         cancel,         
         pollForCancel,
         cancelMe
       )
       where

import Control.Monad.State as S
import Control.Monad.IO.Class 
import Data.IORef

import Control.Par.Class as PC

--------------------------------------------------------------------------------

newtype CancelT m a = CancelT ((StateT CState m) a)
  deriving (Monad, Functor)

unCancelT :: CancelT t t1 -> StateT CState t t1
unCancelT (CancelT m) = m

-- | Each computation has a boolean flag that stays True while it is still live.
--   Also, the state for one computation is linked to the state of children, so that
--   cancellation may be propagated transitively.
newtype CState = CState (IORef CPair)

instance MonadTrans CancelT where
  lift m = CancelT (lift m)

-- TODO/FIXME: Replace MonadIO with something the user can't safely access.
instance MonadIO m => MonadIO (CancelT m) where
  liftIO m = CancelT (lift (liftIO m))

data CPair = CPair !Bool ![CState]

--------------------------------------------------------------------------------

-- | Run a Par monad with the cancellation effect.  Within this computation 
runCancelT :: MonadIO m => CancelT m a -> m a
runCancelT (CancelT st) = do
  ref <- liftIO $ newIORef (CPair True []) 
  evalStateT st (CState ref) 

poll :: (MonadIO m, LVarSched m) => CancelT m Bool
poll = CancelT$ do
  CState ref  <- S.get
  CPair flg _ <- liftIO$ readIORef ref
  return flg

-- | Check with the scheduler to see if the current thread has been canceled, if so
-- stop computing immediately.
pollForCancel :: (MonadIO m, LVarSched m) => CancelT m ()
pollForCancel = do
  b <- poll
  unless b $ cancelMe

-- | Self cancellation.  Equivalent to the parent calling `cancel`.
cancelMe :: (LVarSched m) => CancelT m ()
cancelMe = lift $ returnToSched

-- | The type of cancellable thread identifiers.
type ThreadId = CState

-- | Fork a computation while retaining a handle on it that can be used to cancel it
-- (and all its descendents).
forkCancelable :: (MonadIO m, LVarSched m) => CancelT m () -> CancelT m ThreadId
forkCancelable (CancelT act) = CancelT$ do
--    b <- poll   -- Tradeoff: we could poll once before the atomic op.
--    when b $ do
    CState parentRef <- S.get
    -- Create new child state:
    childRef <- liftIO$ newIORef (CPair True [])    
    live <- liftIO $ 
      atomicModifyIORef' parentRef $ \ orig@(CPair bl ls) ->
        if bl then
          -- Extend the tree by pointing to our child:
          (CPair True (CState childRef : ls), True)
        else -- The current thread has already been canceled: DONT fork:
          (orig, False)
    if live then
       lift $ forkLV (evalStateT act (CState childRef))
      else cancelMe'
    return (CState parentRef)

-- | Issue a cancellation request for a given sub-computation.  It will not be
-- fulfilled immediately, because the cancellation process is cooperative and only
-- happens when the thread(s) check in with the scheduler.
cancel :: (MonadIO m, Monad m) => ThreadId -> CancelT m ()
cancel (CState ref) = do
  -- To cancel a tree of threads, we atomically mark the root as canceled and then
  -- start chasing the children.  After we cancel any node, no further children may
  -- be added to that node.
  chldrn <- liftIO$ atomicModifyIORef' ref $ \ orig@(CPair flg ls) -> 
    if flg 
     then (CPair False [], ls)
     else (orig, [])
  -- We could do this traversal in parallel if we liked...
  forM_ chldrn cancel

cancelMe' :: LVarSched m => StateT CState m ()
cancelMe' = unCancelT cancelMe

instance (ParQuasi m) => ParQuasi (CancelT m) where
  type QPar (CancelT m) = CancelT (QPar m)
--  toQPar (CancelT m) = CancelT undefined

instance (ParSealed m) => ParSealed (CancelT m) where
  type GetSession (CancelT m) = GetSession m
  
instance (MonadIO m, LVarSched m) => LVarSched (CancelT m) where
  type LVar (CancelT m) = LVar m 

  forkLV act = do _ <- forkCancelable act; return ()
    
  newLV act = lift$ newLV act
  
  stateLV lvar =
    let (_::Proxy (m ()), a) = stateLV lvar
    in (Proxy::Proxy((CancelT m) ()), a)

  putLV lv putter = do
    pollForCancel
    lift $ putLV lv putter    

  getLV lv globThresh deltThresh = do
     pollForCancel
     x <- lift $ getLV lv globThresh deltThresh
    -- FIXME: repoll after blocking ONLY:
     pollForCancel
     return x
     
  returnToSched = lift returnToSched

{-
  freezeLV lvar = do
    toQPar pollForCancel
--    lift$ freezeLV lvar
    undefined
-}
