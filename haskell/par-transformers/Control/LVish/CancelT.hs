{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, InstanceSigs,
             MultiParamTypeClasses, NamedFieldPuns, ScopedTypeVariables,
             TypeFamilies, UndecidableInstances, Unsafe #-}

{-# LANGUAGE DataKinds, FlexibleContexts #-}

-- | A module for adding the cancellation capability.
--
--   In its raw form, this is unsafe, because cancelating could cancel something that
--   would have performed a visible side effect.

module Control.LVish.CancelT
{-       (
         -- * The transformer that adds the cancellation capability
         CancelT(), ThreadId, CFut, CFutFate(..),

         -- * Operations specific to CancelT
         runCancelT,

         -- * Forking computations that may be canceled
         forkCancelable, forkCancelableND,

         -- * More detailed interface, separating forking and TID creation
         createTid, forkCancelableWithTid, forkCancelableNDWithTid,

         -- * Performing cancellation, or waiting for results
         readCFut,
         cancel,
         pollForCancel,
         cancelMe,

         -- * Boolean operations with cancellation
         asyncAnd, asyncAndCPS
       )-}
       where

import Control.Applicative (Applicative)
import Control.Monad.State as S
import Data.Coerce
import Data.IORef
-- import Data.LVar.IVar

import Control.Par.Class as PC
import Control.Par.Class.Unsafe (ParMonad (..))
import Control.Par.EffectSigs as E
import qualified Data.Atomics.Counter as C
--------------------------------------------------------------------------------

-- | A Par-monad scheduler transformer that adds the cancellation capability.  To do
-- this, it must track, and periodically poll, extra mutable state for each
-- cancellable computation in the fork-tree.

newtype CancelT (p :: EffectSig -> * -> * -> *) e s a = CancelT ((StateT CState (p e s)) a)

instance PC.ParMonad m => PC.ParMonad (CancelT m) where
  fork (CancelT task) = CancelT $ do
    s0 <- S.get
    S.lift $ PC.fork $ do
      (res,_) <- S.runStateT task s0
      return res

  internalLiftIO m = CancelT (S.lift (internalLiftIO m))

-- | Each computation has a boolean flag that stays True while it is still live.
--   Also, the state for one computation is linked to the state of children, so that
--   cancellation may be propagated transitively.
newtype CState = CState (IORef CPair)

data CPair = CPair !Bool ![CState]

unCancelT :: CancelT p e s a -> StateT CState (p e s) a
unCancelT (CancelT m) = m

-- | Run a Par monad with the cancellation effect.  Within this computation, it
-- is possible to cancel subtrees of computations.
runCancelT :: (PC.ParMonad p) => CancelT p e s a -> p e s a
runCancelT (CancelT st) = do
  ref <- internalLiftIO $ newIORef (CPair True [])
  evalStateT st (CState ref)

-- Check for cancellation of our thread.
poll :: (PC.ParMonad p, LVarSched p) => CancelT p e s Bool
poll = CancelT $ do
  CState ref  <- S.get
  CPair flg _ <- S.lift $ internalLiftIO $ readIORef ref
  return flg

-- | Check with the scheduler to see if the current thread has been canceled, if so
-- stop computing immediately.
pollForCancel :: (PC.ParMonad p, LVarSched p) => CancelT p e s ()
pollForCancel = do
  b <- poll
  unless b $ cancelMe

-- | Self cancellation.  Not the same as the parent calling `cancel`, because
-- cancelMe only cancels the current leaf of the computation tree, not its siblings
-- (not the entire subtree under a forkCancelable, that is).
cancelMe :: (ParMonad p, LVarSched p) => CancelT p e s ()
cancelMe = CancelT $ S.lift returnToSched

-- | The type of cancellable thread identifiers.
type ThreadId = CState

-- | Sometimes it is necessary to have a TID in scope *before* forking the
-- computations in question.  For that purpose, we provide a two-phase interface:
-- `createTid` followed by `forkCancelableWithTid`.
createTid :: (PC.ParMonad p, LVarSched p) => CancelT p e s ThreadId
{-# INLINE createTid #-}
createTid = CancelT $ do
  newSt <- S.lift $ internalLiftIO $ newIORef (CPair True [])
  return (CState newSt)


-- | Add a new computation to an existing cancelable thread.
--
--   Forking multiple computations with the same Tid are permitted; all threads will
--   be canceled as a group.
forkCancelableWithTid
  :: forall (p :: EffectSig -> * -> * -> *) (e :: EffectSig) s a {- m -} .
     (PC.ParIVar p, LVarSched p,
      FutContents p CFutFate, FutContents p a) =>
      ThreadId -> CancelT p (SetReadOnly e) s a ->
      CancelT p e s (CFut p s a)
{-# INLINE forkCancelableWithTid #-}
forkCancelableWithTid tid act =
  -- unsafeCastEffects2 (Proxy::Proxy(GetEffects (ReadOnlyOf (CancelT m))))
  --                    (forkInternal tid act)
  (error "FINISHME")

-- | Futures that may be canceled before the result is available.
data CFut f m a = CFut (Future f m CFutFate) (Future f m a)

-- | (Internal datatype).  In a deterministic scenario, a `CFut` can only be
-- canceled or read, not both.
data CFutFate = Canceled | Completed
  deriving (Show,Read,Eq)

-- | Fork a computation while retaining a handle on it that can be used to cancel it
-- (and all its descendents).  This is equivalent to `createTid` followed by `forkCancelableWithTid`.
--
-- This version is expected to retain /determinism/.  Therefore, the canceled
-- computations must be read only.
--
-- Finally, note that this currently returns a value in the same monad as the child
-- computation.  That is merely a convenience, and to make typing less of a headache.
-- It is expected, in particular, that the user will lift the read-only computation
-- into a non-read-only parent computation at some point.
forkCancelable
  :: forall (p :: EffectSig -> * -> * -> *) (e :: EffectSig) s a .
     (PC.ParIVar p, LVarSched p, FutContents p CFutFate, FutContents p a) =>
     CancelT p (SetReadOnly e) s a ->
     CancelT p e s (ThreadId, CFut p s a)
forkCancelable act = do
--    b <- poll   -- Tradeoff: we could poll once before the atomic op.
--    when b $ do
    tid <- createTid
    fut <- forkCancelableWithTid tid act
    return $! (tid,fut)

-- | This is a version of `forkCancelable` which allows side-effecting computation to
--   be canceled.  Be warned, that is a dangerous business.  Accordingly, this function
--   introduces nondeterminism and must register the "IO effect".
forkCancelableND
  :: forall (p :: EffectSig -> * -> * -> *) (e :: EffectSig) s a .
     (PC.ParIVar p, LVarSched p, GetI e ~ 'I, GetP e ~ 'P,
      FutContents p CFutFate, FutContents p a) =>
     CancelT p e s a -> CancelT p e s (ThreadId, CFut p s a)
-- TODO/FIXME: Should we allow the child computation to not have IO set if it likes?
forkCancelableND act = do
    tid <- createTid
    fut <- forkCancelableNDWithTid tid act
    return $! (tid, fut)

-- | Variant of `forkCancelableWithTid` that works for nondeterministic computations.
forkCancelableNDWithTid
  :: (PC.ParIVar p, LVarSched p, GetI e ~ 'I, GetP e ~ 'P,
      FutContents p CFutFate, FutContents p a) =>
     ThreadId -> CancelT p e s a -> CancelT p e s (CFut p s a)
{-# INLINE forkCancelableNDWithTid #-}
forkCancelableNDWithTid tid act = forkInternal tid act

-- Internal version -- no rules!
forkInternal
  :: forall (p :: EffectSig -> * -> * -> *) e s a f .
     (PC.ParIVar p, LVarSched p, GetP e ~ 'P,
      FutContents p CFutFate, FutContents p a) =>
     ThreadId -> CancelT p e s a -> CancelT p e s (CFut p s a)
{-# INLINE forkInternal #-}
forkInternal (CState childRef) (CancelT act) = CancelT $ do
    -- The following line gives me this GHC panic:
    -- ghc: panic! (the 'impossible' happened)
    --   (GHC version 7.6.3 for x86_64-apple-darwin):
    --         cgLookupPanic (probably invalid Core; try -dcore-lint)
    --     cobox{v aQh} [lid]
    --     static binds for:
    --     local binds for:
    --     main:Control.LVish.CancelT.$WCPair{v rM6} [gid[DataConWrapper]]
--    fate   <- lift (PC.new :: m CFutFate)

    fate   <- S.lift (PC.new :: p e s (Future p s CFutFate))
    result <- S.lift (PC.new :: p e s (Future p s a))

    CState parentRef <- S.get
    -- Create new child state:
    live <- S.lift $ internalLiftIO $
      atomicModifyIORef' parentRef $ \ orig@(CPair bl ls) ->
        if bl then
          -- Extend the tree by pointing to our child:
          (CPair True (CState childRef : ls), True)
        else -- The current thread has already been canceled: DONT fork:
          (orig, False)
    let act' = do x <- act
                  S.lift $ PC.put_ result x
                  return ()
    if live then
       S.lift $ forkLV (evalStateT act' (CState childRef))
      else cancelMe'
    return $! CFut fate result

cancelMe' :: LVarSched p => StateT CState (p e s) ()
cancelMe' = unCancelT cancelMe

-- | Do a blocking read on the result of a cancelable-future.  This is an ERROR if
--   the future has been cancelled.  (And for determinism, the reverse is also true.
--   A cancellation after this read is an error.)
readCFut :: (PC.ParIVar p, GetP e ~ P, GetG e ~ G,
             FutContents p CFutFate, FutContents p a)
         => CFut p s a -> p e s a
readCFut (CFut fate res) = do
  PC.put_ fate Completed -- Possible throw a multiple-put error.
  PC.get res

-- | Issue a cancellation request for a given sub-computation.  It will not be
-- fulfilled immediately, because the cancellation process is cooperative and only
-- happens when the thread(s) check in with the scheduler.
cancel :: (PC.ParMonad p, Monad (p e s), ParMonad (CancelT p), GetP e ~ P)
       => ThreadId -> CancelT p e s ()
cancel = internal_cancel

internal_cancel
  :: (PC.ParMonad p, Monad (p e s), ParMonad (CancelT p))
  => ThreadId -> CancelT p e s ()
internal_cancel (CState ref) = do
  -- To cancel a tree of threads, we atomically mark the root as canceled and then
  -- start chasing the children.  After we cancel any node, no further children may
  -- be added to that node.
  chldrn <- internalLiftIO$ atomicModifyIORef' ref $ \ orig@(CPair flg ls) ->
    if flg
     then (CPair False [], ls)
     else (orig, [])

  -- FIXME: write the fates of all associated futures -- lift $ PC.put_ fate

  -- We could do this traversal in parallel if we liked...
  S.forM_ chldrn internal_cancel

{- FIXME: Looks like ParSealed is gone. Make sure this is not needed and remove.

instance (PC.ParSealed m) => PC.ParSealed (CancelT m) where
  type GetSession (CancelT m) = PC.GetSession m

-}

instance forall p (e :: EffectSig) s .
         (PC.ParMonad (CancelT p), PC.ParIVar p, PC.LVarSched p, ParMonadTrans CancelT) =>
          PC.LVarSched (CancelT p) where
  type LVar (CancelT p) = LVar p

-- FIXME: we shouldn't need to fork a CANCELABLE thread here, should we?
--  forkLV act = do _ <- forkCancelable act; return ()
  forkLV act = PC.fork act

  newLV act = PC.lift $ newLV act

  stateLV lvar =
    let (_::Proxy (p e s ()), a) = stateLV lvar
    in (Proxy, a)

  putLV lv putter = do
    pollForCancel
    PC.lift $ putLV lv putter

  getLV lv globThresh deltThresh = do
     pollForCancel
     x <- PC.lift $ getLV lv globThresh deltThresh
    -- FIXME: repoll after blocking ONLY:
     pollForCancel
     return x

  returnToSched = PC.lift returnToSched

{-

--------------------------------------------------------------------------------

instance MonadTrans CancelT where
  lift m = CancelT (S.lift m)



instance (PC.ParQuasi m qm) => PC.ParQuasi (CancelT m) (CancelT qm) where
  toQPar :: (CancelT m) a -> (CancelT qm) a
  toQPar (CancelT (S.StateT{runStateT})) =
    CancelT $ S.StateT $ toQPar . runStateT

instance (Functor qm, Monad qm, PC.ParMonad m, PC.ParIVar m,
          LVarSched m, LVarSchedQ m qm, PC.ParQuasi (CancelT m) (CancelT qm) ) =>
         PC.LVarSchedQ (CancelT m) (CancelT qm) where

  freezeLV :: forall a d . LVar (CancelT m) a d -> (Proxy ((CancelT m) ()), (CancelT qm) ())
  freezeLV lvar = (Proxy, (do
    let lvar2 :: LVar m a d
        lvar2 = lvar -- This works because of the specific def for "type LVar" in the instance above...
    toQPar (pollForCancel :: CancelT m ())
    let frz :: LVar m a d -> (Proxy (m()), qm ())
        frz x = freezeLV x
    CancelT (lift (snd (frz lvar2)))
    return ()))

instance PC.ParFuture m => PC.ParFuture (CancelT m) where
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
  new       = CancelT$ lift PC.new
  put_ iv v = CancelT$ lift$ PC.put_ iv v

--------------------------------------------------------------------------------

-- | A parallel AND operation that not only signals its output early when it receives
-- a False input, but also attempts to cancel the other, unneeded computation.
--
asyncAnd :: forall p . (PC.ParIVar p, PC.ParLVar p,
                        PC.ParWEffects (CancelT p), -- TEMP
                        FutContents p CFutFate, FutContents p (), FutContents p Bool)
            => ((ReadOnlyOf (CancelT p)) Bool)
            -> ((ReadOnlyOf (CancelT p)) Bool)
            -> CancelT p Bool
asyncAnd lef rig = do
  res <- PC.new
  asyncAndCPS lef rig (PC.put res)
  PC.get res

type SetReadOnly e = Ef NP (GetG e) NF NB NI

-- | A continuation-passing-sytle version of `asyncAnd`.  This version is
-- non-blocking, simply registering a call-back that receives the result of the AND
-- computation.
--
-- This version may be more efficient because it saves the allocation of an
-- additional data structure to synchronize on when waiting on the result.
asyncAndCPS :: forall p . (PC.ParMonad p, PC.ParIVar p, PC.ParLVar p,
                           PC.ParWEffects (CancelT p),
                           FutContents p CFutFate, FutContents p ())
--            => ((CancelT p) Bool) -> (CancelT p Bool)
            => ((ReadOnlyOf (CancelT p)) Bool)
            -> ((ReadOnlyOf (CancelT p)) Bool)
            -> (Bool -> CancelT p ()) -> CancelT p ()
-- Similar to `Control.LVish.Logical.asyncAnd`
asyncAndCPS leftM rightM kont = do
  -- Atomic counter, if we are the second True we write the result:
  cnt <- internalLiftIO$ C.newCounter 0 -- TODO we could share this for 3+-way and.
  let -- launch :: ThreadId -> ThreadId -> CancelT p Bool -> CancelT p (CFut p ())
      launch :: ThreadId -> ThreadId ->
                ((ReadOnlyOf (CancelT p)) Bool) ->
                             (CancelT p) (CFut p ())
      launch mine theirs m =
             forkCancelableWithTid mine $
             -- Here are the possible states:
             -- T?   -- 1
             -- TT   -- 2
             -- F?   -- 100
             -- FT   -- 101
             -- TF   -- 101
             -- FF   -- 200
                let roprox = (Proxy::Proxy(SetReadOnly (GetEffects (CancelT p)))) in
                case law2 (Proxy::Proxy((CancelT p) ())) roprox of
                  MkConstraint ->
                   do b <- m
                      let kont2 x = unsafeCastEffects roprox (kont x)
                      case b of
                        True  -> do n <- internalLiftIO$ C.incrCounter 1 cnt
                                    if n==2
                                      then kont2 True
                                      else return ()
                        False -> -- We COULD assume idempotency and execute kont False twice,
                                 -- but since we have the counter anyway let us dedup:
                                 do n <- internalLiftIO$ C.incrCounter 100 cnt
                                    case n of
                                      100 -> do
                                                unsafeCastEffects roprox
                                                  (internal_cancel theirs :: (CancelT p) ())
                                                kont2 False
                                      101 -> kont2 False
                                      200 -> return ()
  tid1 <- createTid
  tid2 <- createTid
  launch tid1 tid2 leftM
  launch tid2 tid1 rightM
  return ()

-}
