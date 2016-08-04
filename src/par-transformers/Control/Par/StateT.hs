{-# LANGUAGE ScopedTypeVariables, FlexibleInstances,
             MultiParamTypeClasses, UndecidableInstances, CPP #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides a notion of (Splittable) State that is
--   compatible with any Par monad.
--
--   This module provides instances that make StateT-transformed
--   monads into valid Par monads.

module Control.Par.StateT
  (
    -- * Class for state split at fork points
   SplittableState(..)
   -- * Also note exported instances.
  )
  where

{-
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.State.Lazy as SL

-- import qualified Control.Monad.Par.Class as PC
import qualified Control.Par.Class as PC
import Control.Par.Class.Unsafe (ParMonad(..), ParThreadSafe(..))
-}

---------------------------------------------------------------------------------
--- Make Par computations with state work.
--- (TODO: move these instances to a different module.)

-- | A type in `SplittableState` is meant to be added to a Par monad
--   using StateT.  It works like any other state except at `fork`
--   points, where the runtime system splits the state using `splitState`.
--
--   Common examples for applications of `SplittableState` would
--   include (1) routing a splittable random number generator through
--   a parallel computation, and (2) keeping a tree-index that locates
--   the current computation within the binary tree of `fork`s.
--   Also, it is possible to simply duplicate the state at all fork points,
--   enabling "thread local" copies of the state.
--
--   The limitation of this approach is that the splitting method is
--   fixed, and the same at all `fork` points.
class SplittableState a where
  splitState :: a -> (a,a)

----------------------------------------------------------------------------------------------------
-- Strict State:

{-

instance ParThreadSafe p => ParThreadSafe (S.StateT s p) where
  {-# INLINE unsafeParIO #-}
  unsafeParIO io = lift (unsafeParIO io)

instance (SplittableState s, ParMonad p) => ParMonad (S.StateT s p) where
  {-# INLINE fork #-}
  fork (task :: S.StateT s p ()) =
              do s <- S.get
                 let (s1,s2) = splitState s
                 S.put s2
                 lift$ PC.fork $ do S.runStateT task s1; return ()
  {-# INLINE internalLiftIO #-}
  internalLiftIO io = lift (internalLiftIO io)

-- | Adding State to a `ParFuture` monad yields another `ParFuture` monad.
instance (SplittableState s, PC.ParFuture p)
      =>  PC.ParFuture (S.StateT s p)
 where
  type Future (S.StateT s p)        = PC.Future p
  type FutContents (S.StateT s p) a = PC.FutContents p a
  {-# INLINE get #-}
  get = lift . PC.get
  {-# INLINE spawn_ #-}
  spawn_ (task :: S.StateT s p ans) =
    do s <- S.get
       let (s1,s2) = splitState s
       S.put s2                               -- Parent comp. gets one branch.
       lift$ PC.spawn_ $ S.evalStateT task s1   -- Child the other.

-- | Likewise, adding State to a `ParIVar` monad yield s another `ParIVar` monad.
instance (SplittableState s, PC.ParIVar p)
      =>  PC.ParIVar (S.StateT s p)
 where
  {-# INLINE new #-}
  new      = lift PC.new
  {-# INLINE put_ #-}
  put_ v x = lift$ PC.put_ v x
  {-# INLINE newFull_ #-}
  newFull_ = lift . PC.newFull_

----------------------------------------------------------------------------------------------------
-- Lazy State:

-- <DUPLICATE_CODE>

instance ParThreadSafe p => ParThreadSafe (SL.StateT s p) where
  {-# INLINE unsafeParIO #-}
  unsafeParIO io = lift (unsafeParIO io)

instance (SplittableState s, ParMonad p) => ParMonad (SL.StateT s p) where
  {-# INLINE fork #-}
  fork (task :: SL.StateT s p ()) =
              do s <- SL.get
                 let (s1,s2) = splitState s
                 SL.put s2
                 lift$ PC.fork $ do SL.runStateT task s1; return ()
  {-# INLINE internalLiftIO #-}
  internalLiftIO io = lift (internalLiftIO io)

-- | Adding State to a `ParFuture` monad yield s another `ParFuture` monad.
instance (SplittableState s, PC.ParFuture p)
      =>  PC.ParFuture (SL.StateT s p)
 where
  type Future (SL.StateT s p)        = PC.Future p
  type FutContents (SL.StateT s p) a = PC.FutContents p a
  {-# INLINE get #-}
  get = lift . PC.get
  {-# INLINE spawn_ #-}
  spawn_ (task :: SL.StateT s p ans) =
    do s <- SL.get
       let (s1,s2) = splitState s
       SL.put s2                               -- Parent comp. gets one branch.
       lift$ PC.spawn_ $ SL.evalStateT task s1   -- Child the other.

-- | Likewise, adding State to a `ParIVar` monad yield s another `ParIVar` monad.
instance (SplittableState s, PC.ParIVar p)
      =>  PC.ParIVar (SL.StateT s p)
 where
  {-# INLINE new #-}
  new      = lift PC.new
  {-# INLINE put_ #-}
  put_ v x = lift$ PC.put_ v x
  {-# INLINE newFull_ #-}
  newFull_ = lift . PC.newFull_


-- </DUPLICATE_CODE>

-}
