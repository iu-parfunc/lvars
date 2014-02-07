{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators, CPP,
    GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, RankNTypes,
    ConstraintKinds, FlexibleContexts, UndecidableInstances #-}

-- | Type-level effect signatures that characterize the side-effects of a given `Par`
-- computation.

module Control.LVish.EffectSigs 
       (
        -- * A type-level record of effects
        EffectSig(..), 
        -- * Simple boolean flags, each with a disttinct kind to prevent mixups
        Putting(..), Getting(..), Freezing(..), Bumping(..), IOing(..), 

        -- * Manipulating the phantom types of a Par monad 
        GetEffects, SetEffects,
        GetSession, SetSession,

        -- * Accessor and setter functions for EffectSigs
        GetP, GetG, GetF, GetB, GetI,
        SetP, SetG, SetF, SetB, SetI,

        -- * Convenience functions that operato over whole monad types
        SetMP, SetMG, SetMF, SetMB, SetMI
       )
       where
import Control.Monad.Trans.Class

import Control.LVish.Types

--------------------------------------------------------------------------------

-- | Currently-tracked effects.  This is INTERNAL/private datatype and subject to
-- change.  You should use constraints such as `HasGet` rather than directly
-- depending on this, whereever possible.
data EffectSig  = Ef Putting Getting Freezing Bumping IOing
-- | A boolean by another name.
data Putting  = P | NP
-- | A boolean by another name.
data Getting  = G | NG
-- | A boolean by another name.
data Freezing = F | NF
-- | A boolean by another name.
data Bumping  = B | NB
-- | A boolean by another name.
data IOing    = I | NI


--------------------------------------------------------------------------------
-- Effect sigs and their extraction:
--------------------------------------------------------------------------------
{-
-- Would closed type families help anything?
type family GetEffects m where
  GetEffects (Par e s) = e
  GetEffects (trans m) = GetEffects m
-}

-- | Type-level utility function for extracting the `e` part of a valid Par-monad stack.
type family GetEffects (m :: (* -> *)) :: EffectSig
-- This is a bit dangerous, but it does things once-and-for-all for all transformers:
type instance GetEffects (trans (m :: * -> *)) = GetEffects m 
-- type instance GetEffects (CancelT m) = GetEffects m
-- type instance GetEffects (DeadlockT m) = GetEffects m

-- | Type-level utility function for extracting the `s` part of a valid Par-monad stack.
type family GetSession (m :: (* -> *)) :: *
type instance GetSession (trans (m :: * -> *)) = GetSession m 

-- | Type-level utility function for replacing the `s` part of a valid Par-monad stack.
type family SetSession (s :: *) (m :: (* -> *)) :: (* -> *)
type instance SetSession s2 (trans (m :: * -> *)) = trans (SetSession s2 m)

-- | Type-level utility function for replacing the `s` part of a valid Par-monad stack.
type family SetEffects (e::EffectSig) (m :: (* -> *)) :: (* -> *)
type instance SetEffects s2 (trans (m :: * -> *)) = trans (SetEffects s2 m)


-- type instance GetSession (CancelT m)   = GetSession m
-- type instance GetSession (DeadlockT m) = GetSession m
-- type instance SetSession e (CancelT m)   = CancelT   (SetSession e m)
-- type instance SetSession e (DeadlockT m) = DeadlockT (SetSession e m)
-- type instance SetEffects e (CancelT m)   = CancelT   (SetEffects e m)
-- type instance SetEffects e (DeadlockT m) = DeadlockT (SetEffects e m)

-- | Utility for getting just the Put effect flag.
type family GetP (e :: EffectSig) :: Putting
type instance GetP (Ef p g f b i) = p

-- | Utility for getting just the Bump effect flag.
type family GetB (e :: EffectSig) :: Bumping
type instance GetB (Ef p g f b i) = b

-- | Utility for getting just the Freeze effect flag.
type family GetF (e :: EffectSig) :: Freezing
type instance GetF (Ef p g f b i) = f

-- | Utility for getting just the Get effect flag.
type family GetG (e :: EffectSig) :: Getting
type instance GetG (Ef p g f b i) = g

-- | Utility for getting just the IO effect flag.
type family GetI (e :: EffectSig) :: IOing
type instance GetI (Ef p g f b i) = i

-- | Utility for setting just the Put effect flag.
type family SetP (p :: Putting) (e :: EffectSig) :: EffectSig
type instance SetP p2 (Ef p1 g f b i) = (Ef p2 g f b i)

-- | Utility for setting just the Get effect flag.
type family SetG (p :: Getting) (e :: EffectSig) :: EffectSig
type instance SetG g2 (Ef p g f b i) = (Ef p g2 f b i)

-- | Utility for setting just the Freeze effect flag.
type family SetF (p :: Freezing) (e :: EffectSig) :: EffectSig
type instance SetF f2 (Ef p g f b i) = (Ef p g f2 b i)

-- | Utility for setting just the Bump effect flag.
type family SetB (b :: Bumping) (e :: EffectSig) :: EffectSig
type instance SetB b2 (Ef p g f b i) = (Ef p g f b2 i)

-- | Utility for setting just the IO effect flag.
type family SetI (b :: IOing) (e :: EffectSig) :: EffectSig
type instance SetI i2 (Ef p g f b i) = (Ef p g f b i2)

----------------------------------------
-- Same thing but lifted to work over monads:

-- Undecidable instances:

-- | Shorthand for setting the Put effeect of a Par monad.
type family SetMP (p :: Putting) (m :: * -> *) :: (* -> *)
type instance SetMP p m = SetEffects (SetP p (GetEffects m)) m

-- | Shorthand for setting the Get effeect of a Par monad.
type family SetMG (g :: Getting) (m :: * -> *) :: (* -> *)
type instance SetMG g m = SetEffects (SetG g (GetEffects m)) m

-- | Shorthand for setting the Freeze effeect of a Par monad.
type family SetMF (f :: Freezing) (m :: * -> *) :: (* -> *)
type instance SetMF f m = SetEffects (SetF f (GetEffects m)) m

-- | Shorthand for setting the Bump effeect of a Par monad.
type family SetMB (b :: Bumping) (m :: * -> *) :: (* -> *)
type instance SetMB b m = SetEffects (SetB b (GetEffects m)) m

-- | Shorthand for setting the IO effeect of a Par monad.
type family SetMI (i :: IOing) (m :: * -> *) :: (* -> *)
type instance SetMI i m = SetEffects (SetI i (GetEffects m)) m
