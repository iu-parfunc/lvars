{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators, CPP,
    GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, RankNTypes,
    ConstraintKinds, FlexibleContexts, UndecidableInstances #-}

-- | Type-level effect signatures that characterize the side-effects of a given `Par`
-- computation.
--
-- This notion of effect tracking is coarse.  We track whether a given
-- computation may:
--
--  * Put - may mutate one or more variables outside the computation.
--  * Get - may read and thus block on one or more variables outside
--          the scope of the computation.
--  * Frz - may freeze
--  * Bmp - may perform non-idempotent writes
--  * IO  - may perform arbitrary IO effects and thus is nondeterministic.
--
-- These are conservative in the sense that a "Put" means only the
-- possibility of a put, not a guarantee that one may occur.  Thus
-- there is an effect subtype ordering for the above effect signatures
-- in which "pgfbi" is bottom and "PGFBI" is top (all off vs all on).

module Control.Par.EffectSigs
       (
        -- * A type-level record of effects
        EffectSig(..),
        -- * Simple boolean flags, each with a disttinct kind to prevent mixups
        Putting(..), Getting(..), Freezing(..), Bumping(..), IOing(..),

        -- * User-visible Constraints
        HasPut, HasGet, HasFreeze, HasBump, HasIO,
        NoPut, NoGet, NoFreeze, NoBump, NoIO,

        -- * Derived constraints, i.e. shorthands for common combinations:
        ReadOnly, Deterministic, QuasiDeterministic, Idempotent,
        SetReadOnly,

        -- * Accessor and setter functions for EffectSigs
        GetP, GetG, GetF, GetB, GetI,
        SetP, SetG, SetF, SetB, SetI,
       )
       where

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

-- | Utility for getting just the Put effect flag.
type family GetP (e :: EffectSig) :: Putting where
  GetP ('Ef p g f b i) = p

-- | Utility for getting just the Bump effect flag.
type family GetB (e :: EffectSig) :: Bumping where
  GetB ('Ef p g f b i) = b

-- | Utility for getting just the Freeze effect flag.
type family GetF (e :: EffectSig) :: Freezing where
  GetF ('Ef p g f b i) = f

-- | Utility for getting just the Get effect flag.
type family GetG (e :: EffectSig) :: Getting where
  GetG ('Ef p g f b i) = g

-- | Utility for getting just the IO effect flag.
type family GetI (e :: EffectSig) :: IOing where
  GetI ('Ef p g f b i) = i

-- | Utility for setting just the Put effect flag.
type family SetP (p :: Putting) (e :: EffectSig) :: EffectSig where
  SetP p2 ('Ef p1 g f b i) = 'Ef p2 g f b i

-- | Utility for setting just the Get effect flag.
type family SetG (p :: Getting) (e :: EffectSig) :: EffectSig where
  SetG g2 ('Ef p g f b i) = 'Ef p g2 f b i

-- | Utility for setting just the Freeze effect flag.
type family SetF (p :: Freezing) (e :: EffectSig) :: EffectSig where
  SetF f2 ('Ef p g f b i) = 'Ef p g f2 b i

-- | Utility for setting just the Bump effect flag.
type family SetB (b :: Bumping) (e :: EffectSig) :: EffectSig where
  SetB b2 ('Ef p g f b i) = 'Ef p g f b2 i

-- | Utility for setting just the IO effect flag.
type family SetI (b :: IOing) (e :: EffectSig) :: EffectSig where
  SetI i2 ('Ef p g f b i) = 'Ef p g f b i2

-- | Replace the relevant effect bits with those required by `ReadOnly`.
type family SetReadOnly (e :: EffectSig) :: EffectSig where
  SetReadOnly ('Ef p g f b i) = 'Ef 'NP g 'NF 'NB 'NI

-- NOTE: We could consider an alias for this, but it doesn't help
-- teach the GHC type checker theorems like (GetG (SetP e) ~ GetG e).
--
-- type SetReadOnly e = SetP NP (SetF NF (SetB NB (SetI NI e)))

--------------------------------------------------------------------------------
-- Now for constraints:

#if 1
-- APPROACH (1): Type aliases.
-- These have the problem that when you ask the type in GHC (:t),
-- it inlines the aliases, revealing ugly types.
type HasPut e    = (GetP e ~ 'P)
type HasGet e    = (GetG e ~ 'G)
type HasFreeze e = (GetF e ~ 'F)
type HasBump   e = (GetB e ~ 'B)
type HasIO  e    = (GetI e ~ 'I)

type NoPut    e = ('NP ~ GetP e)
type NoGet    e = ('NG ~ GetG e)
type NoBump   e = ('NB ~ GetB e)
type NoFreeze e = ('NF ~ GetF e)
type NoIO     e = ('NI ~ GetI e)

type QuasiDeterministic e = (NoIO e)
type Deterministic e = (NoFreeze e, NoIO e)
type Idempotent    e = (NoBump e,   NoIO e)

-- | A shorthand for several negative constraints.
type ReadOnly e = (NoPut e, NoBump e, NoFreeze e, NoIO e)

#else
      -- APPROACH (2): [Total] type families.

      type family HasPut (e :: EffectSig) :: Constraint
      type instance (HasPut (Ef p g f b i)) = (p ~ P)

      -- Derived constraints, i.e. shorthands for common combinations:
      ----------------------------------------

      type family ReadOnly (e :: EffectSig) :: Constraint
      -- type instance (ReadOnly (Ef p g f b i)) = (p ~ NP, b ~ NB , f ~ NF , i ~ NI)
      type instance (ReadOnly (Ef NP g NF NB NI)) = ()
      -- type instance (ReadOnly e) = (GetP e ~ NP, GetB e ~ NB , GetF e ~ NF , GetI e ~ NI)
      -- type instance (ReadOnly e) = (NoPut e, ..)

      type family Deterministic (e :: EffectSig) :: Constraint
      type instance (ReadOnly (Ef p g NF b NI)) = ()

      type family QuasiDeterministic (e :: EffectSig) :: Constraint
      type instance (ReadOnly (Ef p g nf b NI)) = ()
#endif


----------------------------------------------------------------

