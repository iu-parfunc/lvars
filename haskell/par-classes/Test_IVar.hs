{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

{-# LANGUAGE UndecidableInstances #-} -- For ParFuture instance

{-# LANGUAGE GADTs #-} 

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

module Test_IVar
       {-(
         IVar(..),
         -- * Basic IVar operations, same as in monad-par
         new, get, put, put_,
         
         -- * Derived IVar operations, same as in monad-par
        spawn, spawn_, spawnP,

        -- * LVar-style operations
        freezeIVar, fromIVar, whenFull)-}
       where

import           Data.IORef
import           Control.DeepSeq
import           System.Mem.StableName (makeStableName, hashStableName)
import           System.IO.Unsafe      (unsafePerformIO, unsafeDupablePerformIO)
import qualified Data.Foldable    as F
import           Control.Exception (throw)
-- import qualified Control.LVish as LV 
-- import           Control.LVish.DeepFrz.Internal
-- import qualified Control.LVish.Internal as I
-- import           Control.LVish.Internal (Par(WrapPar), LVar(WrapLVar), Determinism(QuasiDet))
-- import           Control.LVish.SchedIdempotent (newLV, putLV, getLV, freezeLV)
-- import qualified Control.LVish.SchedIdempotent as LI 
import           Data.LVar.Generic
import           Data.LVar.Generic.Internal (unsafeCoerceLVar)
import           GHC.Prim (unsafeCoerce#)

import Control.Par.Class (LVarSched(..))
import qualified Control.Par.Class as PC

------------------------------------------------------------------------------
-- IVars implemented on top of (the idempotent implementation of) LVars
------------------------------------------------------------------------------

-- | An `IVar` is the simplest form of `LVar`.
data IVar lvar s a = forall m qm . LVarSched m qm lvar => IVar !(lvar (IORef (Maybe a)) a)

-- the global data for an IVar a is a reference to Maybe a, while deltas are
-- simply values of type a (taking the IVar from Nothing to Just):

-- | Physical equality, just as with `IORef`s.
instance Eq (IVar m s a) where
   (IVar lv1) == (IVar lv2) = stateLV lv1 == stateLV lv2

eq :: forall m qm lvar s elt . (PC.LVarSched m qm lvar) =>
      IVar lvar s elt -> IVar lvar s elt -> Bool
eq (IVar lv1) (IVar lv2) =
  x == y
 where   
   x :: (IORef (Maybe elt))
   x = PC.stateLV (lv1 :: lvar (IORef (Maybe elt)) elt)
   y :: (IORef (Maybe elt))
   y = PC.stateLV (lv2 :: lvar (IORef (Maybe elt)) elt)
  
{- 

-- | An `IVar` can be treated as a generic container LVar which happens to
-- contain at most one value!  Note, however, that the polymorphic operations are
-- less useful than the monomorphic ones exposed by this module.
instance LVarData1 IVar where  
  freeze :: IVar s a -> Par QuasiDet s (IVar Frzn a)
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
-}

--------------------------------------

{-# INLINE new #-}
-- | A new IVar that starts out empty. 
new :: LVarSched m qm lvar => m (IVar lvar a d)
new = do x <- PC.newLV $ newIORef Nothing
         return (IVar x)


{-# INLINE get #-}
-- | Read the value in a IVar.  The 'get' can only return when the
-- value has been written by a prior or concurrent @put@ to the same
-- IVar.
get :: LVarSched m qm lvar => IVar lvar s a -> m a
get (IVar lv) = getLV lv globalThresh deltaThresh
  where globalThresh ref _ = readIORef ref    -- past threshold iff Jusbt _
        deltaThresh  x     = return $ Just x  -- always past threshold

{-# INLINE put_ #-}
-- | Put a value into an IVar.  Multiple 'put's to the same IVar
-- are not allowed, and result in a runtime error, unless the values put happen to be @(==)@.
--         
-- This function is always at least strict up to WHNF in the element put.
put_ :: LVarSched m qm lvar => Eq a => IVar lvar s a -> a -> m ()
put_ (IVar iv) !x = putLV iv putter
  where putter ref      = atomicModifyIORef ref update
        update (Just y) | x == y = (Just y, Nothing)
                        | otherwise = unsafePerformIO $
                            do n1 <- fmap hashStableName $ makeStableName x
                               n2 <- fmap hashStableName $ makeStableName y
                               -- throw (LV.ConflictingPutExn$ "Multiple puts to an IVar! (obj "++show n2++" was "++show n1++")")
                               error ("Multiple puts to an IVar! (obj "++show n2++" was "++show n1++")")
        update Nothing  = (Just x, Just x)


-- | A specialized freezing operation for IVars that leaves the result in a handy format (`Maybe`).
freezeIVar :: forall m qm lvar s a . LVarSched m qm lvar => IVar lvar s a -> qm (Maybe a)
-- freezeIVar :: LVarSched m lvar => IVar lvar s a -> QPar m (Maybe a)
-- freezeIVar :: forall m qm lvar s a . LVarSched m qm lvar => IVar lvar s a -> m (Maybe a)
freezeIVar (IVar lv) = toQPar p
  where
    p :: m (Maybe a)
    p = do freezeLV lv
           getLV lv globalThresh deltaThresh
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing

{-

-- | Unpack a frozen IVar (as produced by a generic `freeze` operation) as a more
-- palatable data structure.
fromIVar :: IVar Frzn a -> Maybe a
fromIVar (IVar lv) = unsafeDupablePerformIO $ readIORef (I.state lv)

{-# INLINE whenFull #-}
-- | Register a handler that fires when the IVar is filled, which, of course, only
--   happens once.
whenFull :: Maybe LI.HandlerPool -> IVar s a -> (a -> Par d s ()) -> Par d s ()
whenFull mh (IVar (WrapLVar lv)) fn = 
   WrapPar (LI.addHandler mh lv globalCB fn')
  where
    fn' x = return (Just (I.unWrapPar (fn x)))
    globalCB ref = do
      mx <- readIORef ref -- Snapshot
      case mx of
        Nothing -> return Nothing
        Just v  -> fn' v
  
--------------------------------------------------------------------------------

{-# INLINE spawn #-}
-- | A simple future represented as an IVar.  The result is fully evaluated before
-- the child computation returns.
spawn :: (Eq a, NFData a) => Par d s a -> Par d s (IVar s a)
spawn p  = do r <- new;  LV.fork (p >>= put r);   return r

{-# INLINE spawnP #-}
spawnP :: (Eq a, NFData a) => a -> Par d s (IVar s a)
spawnP a = spawn (return a)

-}

#if 1
{-# INLINE spawn_ #-}
-- | A version of `spawn` that uses only WHNF, rather than full `NFData`.
spawn_ :: (LVarSched m qm lvar, Eq a) => m a -> m (IVar lvar s a)
spawn_ p = undefined
  -- do r <- new;  PC.fork (p >>= put_ r);  return r
#endif

{-# INLINE put #-}
-- | Fill an `IVar`.
put :: (LVarSched m qm lvar, Eq a, NFData a) => IVar lvar s a -> a -> m ()
put v a = deepseq a (put_ v a)

-- #ifdef GENERIC_PAR
#if 1
instance LVarSched m qm lvar => PC.ParFuture m where
  type Future m        = IVar (GetLVar m) (GetSession m) 
  type FutContents m a = (Eq a)
  spawn_ = spawn_
  get = get

-- instance PC.ParIVar (Par d s) where
--   fork = LV.fork  
--   put_ = put_
--   new = new

#endif

