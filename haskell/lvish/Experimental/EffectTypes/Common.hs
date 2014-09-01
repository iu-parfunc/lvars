{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators, CPP,
    GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, RankNTypes,
    ConstraintKinds, FlexibleContexts  #-}
{-# LANGUAGE UndecidableInstances #-}    

-- PolyKinds, ImpredicativeTypes
module Common where
import Control.Monad.Trans.Class
import Control.Applicative

--------------------------------------------------------------------------------
-- Dummy definitions:

class ParMonad (m :: * -> *) where
class LVarMonad (m :: * -> *) where
data Determinism = Det | QuasiDet deriving Show

newtype Par :: EffectsSig -> * -> * -> * where
  WrapPar :: Double -> Par d s a
instance Monad (Par efs s) where
  (>>=) = undefined
  return = undefined

instance Applicative (Par efs s) where

instance Functor     (Par efs s) where
  fmap = undefined 

instance LVarMonad (Par efs s) where

data IVar s a = IVar 
data LVar s a = LVar 
data CancelT   (p:: * -> *) a   = CancelT
data DeadlockT (p:: * -> *) a = DeadlockT

instance Monad m => Monad (CancelT m) where
instance ParMonad m => ParMonad (CancelT m) where
instance LVarMonad m => LVarMonad (CancelT m) where
instance Monad m => Monad (DeadlockT m) where
instance ParMonad m => ParMonad (DeadlockT m) where
instance LVarMonad m => LVarMonad (DeadlockT m) where

instance MonadTrans CancelT where
instance MonadTrans DeadlockT where

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
data EffectsSig  = Ef Putting Getting Freezing Bumping IOing
data Putting  = P | NP
data Getting  = G | NG
data Freezing = F | NF
data Bumping  = B | NB
data IOing    = I | NI

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

-- A different attempt:
----------------------------------------
