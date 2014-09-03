{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators, CPP,
    GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, RankNTypes,
    ConstraintKinds, FlexibleContexts, ScopedTypeVariables, RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}    

-- PolyKinds, ImpredicativeTypes
module Common ( Par(), EffectsSig(..), Putting(..), Getting(..), Freezing(..), 
                Bumping(..), IOing(..)
              ) where

import Control.Monad.Trans.Class
-- import qualified Control.Monad.State as S
import Control.Applicative
import Data.Coerce (coerce, Coercible)

data EffectsSig  = Ef Putting Getting Freezing Bumping IOing
data Putting  = P | NP
data Getting  = G | NG
data Freezing = F | NF
data Bumping  = B | NB
data IOing    = I | NI

--------------------------------------------------------------------------------
-- Dummy definitions:

class ParMonad (m :: * -> *) where

class 
     (Coercible (ReadOnlyOf2 m) m) => 
--      (Coercible (SetEffects2 e m) m) => 
--      (Coercible (SetEffects2 (Ef P G F B I) m) m) => 
      LVarMonad (m :: * -> *) where
  -- CHOICE: we could make these type families class associated:
  type GetEffects2 m :: EffectsSig
  type SetEffects2 (e::EffectsSig) m :: (* -> *)

  liftRO :: (ReadOnlyOf2 m) a -> m a

type ReadOnlyOf2 m = (SetEffects2 (Ef NP (GetG (GetEffects2 m)) NF NB NI) m)

data Determinism = Det | QuasiDet deriving Show

newtype Par :: EffectsSig -> * -> * -> * where
  WrapPar :: RealComp e -> Par e s a

type role Par nominal nominal representational


#if 1
instance LVarMonad (Par efs s) where
  type GetEffects2 (Par efs s) = efs  -- Why is this a problem?
  type SetEffects2 efs (Par e2 s) = Par efs s 
  liftRO (WrapPar y) = (WrapPar (coerce y))

#else
instance LVarMonad (Par (Ef p g f b i) s) where
  type GetEffects2 (Par (Ef p g f b i) s) = (Ef p g f b i)
  type SetEffects2 efs (Par (Ef p g f b i) s) = Par efs s 

  -- If the inner RealComp is non-indexed, can just unwrap/rewrap:
  -- liftRO (WrapPar x :: Par (Ef NP g NF NB NI) s a) = 
  --   (WrapPar x) :: Par (Ef p g f b i) s a

  -- If it is indexed, we have to use safe coercion:
  liftRO (WrapPar x :: Par (Ef NP g NF NB NI) s a) = 
    (WrapPar (coerce x)) :: Par (Ef p g f b i) s a
#endif

instance Monad (Par efs s) where
  (>>=) = undefined
  return = undefined

instance Applicative (Par efs s) where
  (<*>) = undefined 
  pure  = undefined

instance Functor     (Par efs s) where
  fmap = undefined 

data RealComp (e::EffectsSig) = RealComp -- Dummy placeholder.

data IVar s a = IVar 
data LVar s a = LVar 
data DeadlockT (p:: * -> *) a = DeadlockT

-- Here we just capture the notion that it's a state monad
newtype CancelT (p:: * -> *) a  = CancelT (Int -> p (a,Int))

instance Functor m => Functor (CancelT m) where
  fmap = undefined
instance Applicative m => Applicative (CancelT m) where
  (<*>) = undefined
  pure = undefined
instance Monad m => Monad (CancelT m) where
  return = undefined
  (>>=)  = undefined

instance ParMonad m => ParMonad (CancelT m) where

instance (LVarMonad m) => LVarMonad (CancelT m) where
  type GetEffects2 (CancelT m)     = GetEffects2 m
  type SetEffects2 efs (CancelT m) = CancelT (SetEffects2 efs m)
  
  -- Simple case, if we have a computation in the inner monad, we can do it:
  -- This SHOULD be a zero-cost coerce, however!!
  liftRO (CancelT fn) = CancelT (\s -> liftRO (fn s))
  -- liftRO = unsafeCoerce
  -- liftRO x = coerce x b

instance Functor m => Functor (DeadlockT m) where
  fmap = undefined
instance Applicative m => Applicative (DeadlockT m) where
  (<*>) = undefined
  pure = undefined
instance Monad m => Monad (DeadlockT m) where
  return = undefined
  (>>=)  = undefined
-- instance ParMonad m => ParMonad (DeadlockT m) where
-- instance LVarMonad m => LVarMonad (DeadlockT m) where

instance MonadTrans CancelT where
  lift = undefined
instance MonadTrans DeadlockT where
  lift = undefined

data TID = TID

--------------------------------------------------------------------------------
-- Effect sigs and their extraction:
--------------------------------------------------------------------------------
{-
type family GetEffects m where
  GetEffects (Par e s) = e
  GetEffects (trans m) = GetEffects m
-}

type family GetEffects (m :: (* -> *)) :: EffectsSig
type instance GetEffects (Par e s) = e
type instance GetEffects (trans (m :: * -> *)) = GetEffects m
-- type instance GetEffects (CancelT m) = GetEffects m
-- type instance GetEffects (DeadlockT m) = GetEffects m

type family GetSession (m :: (* -> *)) :: *
type instance GetSession (Par e s) = s
type instance GetSession (CancelT m)   = GetSession m
type instance GetSession (DeadlockT m) = GetSession m

type family SetSession (s :: *) (m :: (* -> *)) :: (* -> *)
type instance SetSession s2 (Par e s) = Par e s2
type instance SetSession e (CancelT m)   = CancelT   (SetSession e m)
type instance SetSession e (DeadlockT m) = DeadlockT (SetSession e m)

type family SetEffects (e::EffectsSig) (m :: (* -> *)) :: (* -> *)
type instance SetEffects e2 (Par e1 s) = Par e2 s
type instance SetEffects e (CancelT m)   = CancelT   (SetEffects e m)
type instance SetEffects e (DeadlockT m) = DeadlockT (SetEffects e m)

type family GetP (e :: EffectsSig) :: Putting
type instance GetP (Ef p g f b i) = p

type family GetB (e :: EffectsSig) :: Bumping
type instance GetB (Ef p g f b i) = b

type family GetF (e :: EffectsSig) :: Freezing
type instance GetF (Ef p g f b i) = f

type family GetG (e :: EffectsSig) :: Getting
type instance GetG (Ef p g f b i) = g

type family GetI (e :: EffectsSig) :: IOing
type instance GetI (Ef p g f b i) = i

type family SetP (p :: Putting) (e :: EffectsSig) :: EffectsSig
type instance SetP p2 (Ef p1 g f b i) = (Ef p2 g f b i)

type family SetG (p :: Getting) (e :: EffectsSig) :: EffectsSig
type instance SetG g2 (Ef p g f b i) = (Ef p g2 f b i)

type family SetF (p :: Freezing) (e :: EffectsSig) :: EffectsSig
type instance SetF f2 (Ef p g f b i) = (Ef p g f2 b i)

type family SetB (b :: Bumping) (e :: EffectsSig) :: EffectsSig
type instance SetB b2 (Ef p g f b i) = (Ef p g f b2 i)

type family SetI (b :: IOing) (e :: EffectsSig) :: EffectsSig
type instance SetI i2 (Ef p g f b i) = (Ef p g f b i2)

----------------------------------------
-- Same thing but lifted to work over monads:

-- Undecidable instances:
type family SetMP (p :: Putting) (m :: * -> *) :: (* -> *)
type instance SetMP p m = SetEffects (SetP p (GetEffects m)) m

type family SetMG (g :: Getting) (m :: * -> *) :: (* -> *)
type instance SetMG g m = SetEffects (SetG g (GetEffects m)) m

type family SetMF (f :: Freezing) (m :: * -> *) :: (* -> *)
type instance SetMF f m = SetEffects (SetF f (GetEffects m)) m

type family SetMB (b :: Bumping) (m :: * -> *) :: (* -> *)
type instance SetMB b m = SetEffects (SetB b (GetEffects m)) m

type family SetMI (i :: IOing) (m :: * -> *) :: (* -> *)
type instance SetMI i m = SetEffects (SetI i (GetEffects m)) m


----------------------------------------
-- Tests:

t0 :: Par (Ef NP g NF NB NI) s Int
t0 = return 3

t1 :: Par (Ef p g f b i) s Int
t1 = liftRO t0

t2 :: Par (Ef p g f b i) s Int
t2 = coerce t0


x = coerce :: Par (Ef NP NG NF NB NI) s a -> Par (Ef P G F B I) s a 

_ = coerce :: forall p g f b i s a . Par (Ef NP NG NF NB NI) s a -> Par (Ef p g f b i) s a 

-- x = coerce :: Int -> Bool
-- y = coerce :: Test Int -> Test Bool

data Test x = Test x
