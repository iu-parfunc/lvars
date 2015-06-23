{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-|

  IVars are the very simplest form of LVars.  They are either empty or full, and
  contain at most a single value.

  For further information on using IVars in Haskell, see the @monad-par@ and
  @meta-par@ packages and papers:

    * <http://hackage.haskell.org/package/monad-par>

    * <http://www.cs.indiana.edu/~rrnewton/papers/haskell2011_monad-par.pdf>

    * <http://hackage.haskell.org/package/meta-par>

    * <http://www.cs.indiana.edu/~rrnewton/papers/2012-ICFP_meta-par.pdf>

Unlike the @IVar@ type provided by @monad-par@, the 'IVar' type
provided in this module permits repeated `put`s of the same value, in
keeping with the lattice-based semantics of LVars in which a `put`
takes the least upper bound of the old and new values.

 -}

module Data.LVar.IVar
       (
         IVar(..),
         -- * Basic IVar operations, same as in monad-par
         new, get, put, put_,

         -- * Derived IVar operations, same as in monad-par
        spawn, spawn_, spawnP,

        -- * LVar-style operations
        freezeIVar, fromIVar, whenFull)
       where

import           Control.DeepSeq
import           Control.Exception                      (throw)
import qualified Control.LVish.Basics                   as LV
import           Control.LVish.DeepFrz.Internal
import           Control.LVish.Internal                 (LVar (WrapLVar),
                                                         Par (WrapPar))
import qualified Control.LVish.Internal                 as I
import qualified Control.LVish.Types                    as LV
import           Control.Par.EffectSigs
import qualified Data.Foldable                          as F
import           Data.IORef
import           Data.LVar.Generic
import           Data.LVar.Generic.Internal             (unsafeCoerceLVar)
import           GHC.Prim                               (unsafeCoerce#)
import           Internal.Control.LVish.SchedIdempotent (freezeLV, getLV, newLV,
                                                         putLV)
import qualified Internal.Control.LVish.SchedIdempotent as LI
import           System.IO.Unsafe                       (unsafeDupablePerformIO,
                                                         unsafePerformIO)
import           System.Mem.StableName                  (hashStableName,
                                                         makeStableName)

import qualified Control.Par.Class        as PC
import qualified Control.Par.Class.Unsafe as PC

------------------------------------------------------------------------------
-- IVars implemented on top of (the idempotent implementation of) LVars
------------------------------------------------------------------------------

-- | An `IVar` is the simplest form of `LVar`.
newtype IVar s a = IVar (LVar s (IORef (Maybe a)) a)
-- the global data for an IVar a is a reference to Maybe a, while deltas are
-- simply values of type a (taking the IVar from Nothing to Just):

-- | Physical equality, just as with `IORef`s.
instance Eq (IVar s a) where
  (==) (IVar lv1) (IVar lv2) = I.state lv1 == I.state lv2

-- | An `IVar` can be treated as a generic container LVar which happens to
-- contain at most one value!  Note, however, that the polymorphic operations are
-- less useful than the monomorphic ones exposed by this module.
instance LVarData1 IVar where
  freeze :: HasFreeze e => IVar s a -> Par e s (IVar Frzn a)
  freeze orig@(IVar (WrapLVar lv)) = WrapPar $ do
    freezeLV lv
    return (unsafeCoerceLVar orig)
  addHandler = whenFull

instance LVarWBottom IVar where
  type LVContents IVar a = ()
  newBottom = new

-- Just a type-coercion.  No bits flipped at runtime.
instance DeepFrz a => DeepFrz (IVar s a) where
  type FrzType (IVar s a) = IVar Frzn (FrzType a)
  frz = unsafeCoerceLVar

-- As with all other `Trvrsbl` LVars, the elements are traversable in
-- a fixed order.
instance F.Foldable (IVar Trvrsbl) where
  foldr fn zer (IVar lv) =
    case unsafeDupablePerformIO$ readIORef (I.state lv) of
      Just x  -> fn x zer
      Nothing -> zer

instance (Show a) => Show (IVar Frzn a) where
  show (IVar lv) =
    show $ unsafeDupablePerformIO $ readIORef (I.state lv)

-- | For convenience only; the user could define this.
instance Show a => Show (IVar Trvrsbl a) where
  show = show . castFrzn

--------------------------------------

{-# INLINE new #-}
-- | A new IVar that starts out empty.
new :: Par e s (IVar s a)
new = WrapPar$ fmap (IVar . WrapLVar) $
      newLV $ newIORef Nothing

{-# INLINE get #-}
-- | Read the value in a IVar.  The 'get' can only return when the
-- value has been written by a prior or concurrent @put@ to the same
-- IVar.
get :: HasGet e => IVar s a -> Par e s a
get (IVar (WrapLVar iv)) = WrapPar$ getLV iv globalThresh deltaThresh
  where globalThresh ref _ = readIORef ref    -- past threshold iff Just _
        deltaThresh  x     = return $ Just x  -- always past threshold

{-# INLINE put_ #-}
-- | Put a value into an IVar.  Multiple 'put's to the same IVar
-- are not allowed, and result in a runtime error, unless the values put happen to be @(==)@.
--
-- This function is always at least strict up to WHNF in the element put.
put_ :: (HasPut e, Eq a) => IVar s a -> a -> Par e s ()
put_ (IVar (WrapLVar iv)) !x = WrapPar $ putLV iv putter
  where putter ref      = atomicModifyIORef' ref update
        update Nothing  = (Just x, Just x)
        update (Just y) | x == y = (Just y, Nothing)
                        | otherwise = unsafePerformIO $
                            do n1 <- fmap hashStableName $ makeStableName x
                               n2 <- fmap hashStableName $ makeStableName y
                               throw (LV.ConflictingPutExn$ "Multiple puts to an IVar! (obj "++
                                      show n2++" was "++show n1++")")

putNI_ :: (HasPut e) => IVar s a -> a -> Par e s ()
putNI_ (IVar (WrapLVar iv)) !x = WrapPar $ putLV iv putter
  where putter ref      = atomicModifyIORef' ref update
        update Nothing  = (Just x, Just x)
        update (Just y) = unsafePerformIO $
                            do n1 <- fmap hashStableName $ makeStableName x
                               n2 <- fmap hashStableName $ makeStableName y
                               throw (LV.ConflictingPutExn$ "Multiple puts to an IVar! (obj "++
                                      show n2++" was "++show n1++
                                      ").  putNI_ used, so no multiple-equal-puts allowed.")



-- | A specialized freezing operation for IVars that leaves the result in a handy format (`Maybe`).
freezeIVar :: HasFreeze e => IVar s a -> I.Par e s (Maybe a)
freezeIVar (IVar (WrapLVar lv)) = WrapPar $
   do freezeLV lv
      getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing

-- | Unpack a frozen IVar (as produced by a generic `freeze` operation) as a more
-- palatable data structure.
fromIVar :: IVar Frzn a -> Maybe a
fromIVar (IVar lv) = unsafeDupablePerformIO $ readIORef (I.state lv)

{-# INLINE whenFull #-}
-- | Register a handler that fires when the IVar is filled, which, of course, only
--   happens once.
--
--   Note that like `fork`, adding handlers doesn't introduce specifically tracked
--   effects in the `e` signature.
whenFull :: Maybe LI.HandlerPool -> IVar s a -> (a -> Par e s ()) -> Par e s ()
whenFull mh (IVar (WrapLVar lv)) fn =
   WrapPar (LI.addHandler mh lv globalCB fn')
  where
    -- The threshold is ALWAYS met when a put occurs:
    -- FIXME: That doesn't mean we should always return Just however...
    fn' x = return (Just (I.unWrapPar (fn x)))
    globalCB ref = do
      mx <- LI.liftIO $ readIORef ref -- Snapshot
      case mx of
        Nothing -> return ()
        Just v  -> I.unWrapPar$ fn v

--------------------------------------------------------------------------------

-- TODO: introduce a distinct future type which we can wait on WITHOUT incurring a
-- HasGet constraint...

{-# INLINE spawn #-}
-- | A simple future represented as an IVar.  The result is fully evaluated before
-- the child computation returns.
spawn :: (HasPut e, Eq a, NFData a) => Par e s a -> Par e s (IVar s a)
spawn p  = do r <- new;  LV.fork (p >>= put r);   return r

-- FIXME: Not true yet, but should be:

-- Note that while the implementation of spawn does an IVar `put`, internally,
-- `HasPut` need not appear in the signature, because no puts can possible happen to
-- memory locations visible outside of


{-# INLINE spawn_ #-}
-- | A version of `spawn` that uses only WHNF, rather than full `NFData`.
spawn_ :: (HasPut e, Eq a) => Par e s a -> Par e s (IVar s a)
spawn_ p = do r <- new;  LV.fork (p >>= put_ r);  return r

{-# INLINE spawnP #-}
spawnP :: (HasPut e, Eq a, NFData a) => a -> Par e s (IVar s a)
spawnP a = spawn (return a)

{-# INLINE put #-}
-- | Fill an `IVar`.
put :: (HasPut e, Eq a, NFData a) => IVar s a -> a -> Par e s ()
put v a = deepseq a (put_ v a)


instance PC.ParFuture Par where
  type Future Par = IVar
  type FutContents Par a = (Eq a)
  spawn_ f = spawn_ f
  get iv = get iv

instance PC.ParIVar Par where
  put_ = put_
  putNI_ = putNI_
  new = new
