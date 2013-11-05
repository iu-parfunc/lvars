{-# LANGUAGE Unsafe #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, InstanceSigs #-}

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
         forkCancelable, createTid, forkCancelableWithTid,
         cancel,         
         pollForCancel,
         cancelMe,

         -- * Boolean operations with cancellation
         asyncAnd
       )
       where

import Control.Monad.State as S
import Data.IORef

import Control.Par.Class as PC
import Control.Par.Class.Unsafe (PrivateMonadIO(..))

import qualified Data.Atomics.Counter as C
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

-- TODO/FIXME: Replace PrivateMonadIO with something the user can't safely access.
instance PrivateMonadIO m => PrivateMonadIO (CancelT m) where
  internalLiftIO m = CancelT (lift (internalLiftIO m))

instance PrivateMonadIO m => PrivateMonadIO (StateT s m) where
  internalLiftIO io = lift (internalLiftIO io)

data CPair = CPair !Bool ![CState]

--------------------------------------------------------------------------------

-- | Run a Par monad with the cancellation effect.  Within this computation 
runCancelT :: PrivateMonadIO m => CancelT m a -> m a
runCancelT (CancelT st) = do
  ref <- internalLiftIO $ newIORef (CPair True []) 
  evalStateT st (CState ref) 

poll :: (PrivateMonadIO m, LVarSched m) => CancelT m Bool
poll = CancelT$ do
  CState ref  <- S.get
  CPair flg _ <- internalLiftIO$ readIORef ref
  return flg

-- | Check with the scheduler to see if the current thread has been canceled, if so
-- stop computing immediately.
pollForCancel :: (PrivateMonadIO m, LVarSched m) => CancelT m ()
pollForCancel = do
  b <- poll
  unless b $ cancelMe

-- | Self cancellation.  Equivalent to the parent calling `cancel`.
cancelMe :: (LVarSched m) => CancelT m ()
cancelMe = lift $ returnToSched

-- | The type of cancellable thread identifiers.
type ThreadId = CState

-- | Fork a computation while retaining a handle on it that can be used to cancel it
-- (and all its descendents).  This is equivalent to `createTid` followed by `forkCancelableWithTid`.
forkCancelable :: (PrivateMonadIO m, LVarSched m) => CancelT m () -> CancelT m ThreadId
forkCancelable act = do
--    b <- poll   -- Tradeoff: we could poll once before the atomic op.
--    when b $ do
    tid <- createTid
    forkCancelableWithTid tid act
    return tid

-- | Sometimes it is necessary to have a TID in scope *before* forking the
-- computations in question.  For that purpose, we provide a two-phase interface:
-- `createTid` followed by `forkCancelableWithTid`.
createTid :: (PrivateMonadIO m, LVarSched m) => CancelT m ThreadId
{-# INLINE createTid #-}
createTid = CancelT$ do
    newSt <- internalLiftIO$ newIORef (CPair True [])
    return (CState newSt)

-- | Fork a thread whil emaking it cancelable with the provided threadId argument.
--   Forking multiple computations with the same Tid are permitted; all threads will
--   be canceled as a group.
forkCancelableWithTid :: (PrivateMonadIO m, LVarSched m) => ThreadId -> CancelT m () -> CancelT m ()
{-# INLINE forkCancelableWithTid #-}
forkCancelableWithTid (CState childRef) (CancelT act) = CancelT$ do
    CState parentRef <- S.get
    -- Create new child state:  
    live <- internalLiftIO $ 
      atomicModifyIORef' parentRef $ \ orig@(CPair bl ls) ->
        if bl then
          -- Extend the tree by pointing to our child:
          (CPair True (CState childRef : ls), True)
        else -- The current thread has already been canceled: DONT fork:
          (orig, False)
    if live then
       lift $ forkLV (evalStateT act (CState childRef))
      else cancelMe'
    return ()


-- | Issue a cancellation request for a given sub-computation.  It will not be
-- fulfilled immediately, because the cancellation process is cooperative and only
-- happens when the thread(s) check in with the scheduler.
cancel :: (PrivateMonadIO m, Monad m) => ThreadId -> CancelT m ()
cancel (CState ref) = do
  -- To cancel a tree of threads, we atomically mark the root as canceled and then
  -- start chasing the children.  After we cancel any node, no further children may
  -- be added to that node.
  chldrn <- internalLiftIO$ atomicModifyIORef' ref $ \ orig@(CPair flg ls) -> 
    if flg 
     then (CPair False [], ls)
     else (orig, [])
  -- We could do this traversal in parallel if we liked...
  forM_ chldrn cancel

cancelMe' :: LVarSched m => StateT CState m ()
cancelMe' = unCancelT cancelMe
  
instance (ParSealed m) => ParSealed (CancelT m) where
  type GetSession (CancelT m) = GetSession m
  
instance (PrivateMonadIO m, ParIVar m, LVarSched m) => LVarSched (CancelT m) where
  type LVar (CancelT m) = LVar m 

-- FIXME: we shouldn't need to fork a CANCELABLE thread here, should we?
--  forkLV act = do _ <- forkCancelable act; return ()
  forkLV act = PC.fork act
    
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

instance (ParQuasi m qm) => ParQuasi (CancelT m) (CancelT qm) where
  toQPar :: (CancelT m) a -> (CancelT qm) a 
  toQPar (CancelT (S.StateT{runStateT})) =
    CancelT $ S.StateT $ toQPar . runStateT

instance (Functor qm, Monad qm, PrivateMonadIO m, ParIVar m, 
          LVarSched m, LVarSchedQ m qm, ParQuasi (CancelT m) (CancelT qm) ) =>
         LVarSchedQ (CancelT m) (CancelT qm) where

  freezeLV :: forall a d . LVar (CancelT m) a d -> (Proxy ((CancelT m) ()), (CancelT qm) ())
  freezeLV lvar = (Proxy, (do
    let lvar2 :: LVar m a d
        lvar2 = lvar -- This works because of the specific def for "type LVar" in the instance above...
    toQPar (pollForCancel :: CancelT m ())
    let frz :: LVar m a d -> (Proxy (m()), qm ())
        frz x = freezeLV x 
    CancelT (lift (snd (frz lvar2)))
    return ()))


instance ParFuture m => ParFuture (CancelT m) where
  type Future (CancelT m) = Future m
  type FutContents (CancelT m) a = PC.FutContents m a
  spawn_ (CancelT task) = CancelT $ do
     s0 <- S.get
     lift $ PC.spawn_ $ do
           -- This spawned computation is part of the same tree for purposes of
           -- cancellation:
           (res,_) <- S.runStateT task s0
           return res     
  get iv = CancelT $ lift $ PC.get iv

instance PC.ParIVar m => PC.ParIVar (CancelT m) where
  fork (CancelT task) = CancelT $ do
    s0 <- S.get
    lift $ PC.fork $ do      
      (res,_) <- S.runStateT task s0
      return res
  new       = CancelT$ lift PC.new
  put_ iv v = CancelT$ lift$ PC.put_ iv v

--------------------------------------------------------------------------------

-- UNFINISHED:
-- asyncAnd :: forall p . (PrivateMonadIO p, ParIVar p)
--             => (p Bool) -> (p Bool) -> (Bool -> p ()) -> p ()

asyncAnd :: forall p . (PrivateMonadIO p, ParIVar p, LVarSched p)
            => ((CancelT p) Bool) -> (CancelT p Bool) -> (Bool -> CancelT p ()) -> CancelT p ()  
asyncAnd trueM falseM kont = do
  -- Atomic counter, if we are the second True we write the result:
  cnt <- internalLiftIO$ C.newCounter 0 -- TODO we could share this for 3+-way and.
  let launch mine theirs m =
             forkCancelableWithTid mine $
                   do b <- m
                      case b of
                        True  -> do n <- internalLiftIO$ C.incrCounter 1 cnt
                                    if n==2
                                      then kont True
                                      else return ()
                        False -> -- We COULD assume idempotency and execute kont False twice,
                                 -- but since we have the counter anyway let us dedup:
                                 do n <- internalLiftIO$ C.incrCounter 100 cnt
                                    if n < 200 -- Zero ops or one True.  (If false, nothing to do)
                                      then do -- logDbgLn 1 "asyncAnd: False result, cancelling other..." 
                                              cancel theirs
                                              kont False
                                      else return ()
  tid1 <- createTid
  tid2 <- createTid  
  launch tid1 tid2 trueM
  launch tid2 tid1 falseM

  return ()


