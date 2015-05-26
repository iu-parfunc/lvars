{-|

  The @lvish@ package provides a parallel programming model based on monotonically
  growing data structures.

  This module provides the core scheduler and basic control flow
  operations.  But to do anything useful you will need to import, along
  with this module, one of the data structure modules (@Data.LVar.*@).

  Here is a self-contained example. This program writes the same value
  to an @LVar@ called @num@ twice.  It deterministically prints @4@
  instead of raising an error, as it would if @num@ were a traditional
  IVar rather than an LVar. (You will need to compile using the
  @-XDataKinds@ extension.)

> {-# LANGUAGE DataKinds #-}
> import Control.LVish  -- Generic scheduler; works with any lattice.
> import Data.LVar.IVar -- The particular lattice in question.
> 
> p :: Par Det s Int
> p = do
>   num <- new
>   fork $ put num 4
>   fork $ put num 4
>   get num
> 
> main = do
>   print $ runPar $ p

 -}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- For Deterministic e superclass constraint.

{-# LANGUAGE MultiParamTypeClasses #-}

-- This module reexports the default LVish scheduler, adding some type-level
-- wrappers to ensure propert treatment of determinism.
module Control.LVish
  (
    -- * CRITICAL OBLIGATIONS for the user: valid @Eq@ and total @Ord@
    {-| 
    We would like to tell you that if you're programming with Safe Haskell (@-XSafe@),
    that this library provides a formal guarantee that anything executed with `runPar` is
    guaranteed-deterministic.  Unfortunately, as of this release there is still one back-door
    that hasn't yet been closed.

    If an adversarial user defines invalid `Eq` instances (claiming objects are equal when they're
    not), or if they define a `compare` function that is not a /pure, total function/,
    and then they store those types within `LVar`s,
    then nondeterminism may leak out of a parallel `runPar` computation.

    In future releases, we will strive to require alternate, safe versions of `Eq` and
    `Ord` that are derived automatically by our library and by the GHC compiler.
    -}

    -- * Par computations and their parameters
    Par(), 
    LVishException(..),

    -- * Running various Par computations
    runPar, runParQuasiDet, runParNonDet,
    -- * More polymorphic variants of same
    runParPoly, runParPolyIO, 

    -- * Effect signature manipulation and conversion
    module Control.Par.EffectSigs,
    liftQD,

    -- * Combinators for manually constraining the type of a given Par computation
    isDet, isQD, isND, isIdemD, isIdemQD, isReadOnly,
    hasPut, hasFreeze, hasBump, hasGet, hasIO,
    noPut, noFreeze, noBump, noGet, noIO,

    -- * Subtyping, add more effects to the signature
    -- | These effects are a conservative approximation, therefore it is always ok,
    --   for example, to turn "no put" (`NP`) into "put" (`P`).
    addP, addG, addF, addB, addI, liftReadOnly,
    
    -- * Basic control flow
    fork, yield, 
    --    quiesceAll,    

    -- * Lifting IO, sacrifices determinism
    parIO,
     -- * Various loop constructs
     parForL, parForSimple, parForTree, parForTiled, for_,

     asyncForEachHP,

     -- * Logical control flow operators
     module Control.LVish.Logical,
     -- asyncAnd, asyncOr, andMap, orMap,

     -- * Synchronizing with handler pools
     L.HandlerPool(),    
     newPool, 
     withNewPool, withNewPool_, 
     quiesce, 

     forkHP,

     -- * Reexport IVar operations for a full, standard "Par Monad" API
     module Data.LVar.IVar,

     -- * Debug facilities and internal bits
     logDbgLn, dbgChatterOnly, getLogger, runParLogged, runParDetailed,
     OutDest(..), DbgCfg (..), defaultMemDbgRange,
     LVar()
   ) where

-- NOTE : This is an aggregation module:
import           Control.LVish.Types
import           Control.LVish.Internal as I
import           Control.LVish.Basics as B
import           Control.LVish.Logical
import qualified Internal.Control.LVish.SchedIdempotent as L
import           Control.LVish.SchedIdempotentInternal (State)
import           Control.Par.EffectSigs
import           Control.LVish.Logging (LogMsg(..), OutDest(..), defaultMemDbgRange, logOn)
import           Data.LVar.IVar 
import           Data.Proxy
import           Data.Coerce (coerce)

import Data.IORef
--------------------------------------------------------------------------------

import qualified Control.Par.Class as PC
import qualified Control.Par.Class.Unsafe as PU
    
instance PC.LVarSched Par where
  type LVar Par = L.LVar 

  forkLV = fork
  newLV  = WrapPar . L.newLV
  getLV lv glob delt = WrapPar $ L.getLV lv glob delt
  putLV lv putter    = WrapPar $ L.putLV lv putter

  stateLV (L.LVar{L.state=s}) = (PC.Proxy,s)

  returnToSched = WrapPar $ mkPar $ \_k -> L.sched

instance PU.ParThreadSafe Par where
  unsafeParIO = I.liftIO

-- instance PC.ParLVar (Par e s) where

-- | Lifting IO into `Par` in a manner that is fully accounted for in the effect
-- signature.
parIO :: HasIO e => IO a -> Par e s a
parIO = I.liftIO

-- | Exactly like `logDbgLn` except used for informational chatter
-- only, NOT to signal that a given thread is about to read or modify
-- a memory address.
dbgChatterOnly :: Int -> String -> Par e s ()
dbgChatterOnly lvl msg = do
  x <- getLogger
  case x of
    Nothing  -> logDbgLn lvl msg
    Just lgr -> liftIO $ logOn lgr (OffTheRecord lvl msg)
        

------ DUPLICATED: -----
mkPar :: ((a -> L.ClosedPar) -> SchedState -> IO ()) -> L.Par a
mkPar f = L.Par $ \k -> L.ClosedPar $ \q -> f k q
type SchedState = State L.ClosedPar LVarID
type LVarID = IORef ()

------------------------------------------------------------

hasPut :: HasPut e => Par e s a -> Par e s a
hasPut x = x

hasFreeze :: HasFreeze e => Par e s a -> Par e s a
hasFreeze x = x

hasBump :: HasBump e => Par e s a -> Par e s a
hasBump x = x

hasIO :: HasIO e => Par e s a -> Par e s a
hasIO x = x

hasGet :: HasGet e => Par e s a -> Par e s a
hasGet x = x


noPut :: NoPut e => Par e s a -> Par e s a
noPut x = x

noFreeze :: NoFreeze e => Par e s a -> Par e s a
noFreeze x = x

noBump :: NoBump e => Par e s a -> Par e s a
noBump x = x

noIO :: NoIO e => Par e s a -> Par e s a
noIO x = x

noGet :: NoGet e => Par e s a -> Par e s a
noGet x = x



isDet :: (e ~ (Ef P G NF B NI)) => Par e s a -> Par e s a
isDet x = x

isQD :: (e ~ (Ef P G F B NI)) => Par e s a -> Par e s a
isQD x = x

isND :: (e ~ (Ef P G F B I)) => Par e s a -> Par e s a
isND x = x

isIdemD :: (e ~ (Ef P G NF NB NI)) => Par e s a -> Par e s a
isIdemD x = x

isIdemQD :: (e ~ (Ef P G F NB NI)) => Par e s a -> Par e s a
isIdemQD x = x

isReadOnly :: (e ~ (Ef NP G NF NB NI)) => Par e s a -> Par e s a
isReadOnly x = x

-- | Lift a read-only computation to participate in a parent computation with more
-- effects.
liftReadOnly :: Par (Ef NP g NF NB NI) s a -> Par (Ef p g f b i) s a
liftReadOnly (WrapPar p) = WrapPar p


addP :: Par (Ef NP g f b i) s a -> Par (Ef p g f b i) s a
addP (WrapPar p) = WrapPar p

addG :: Par (Ef p NG f b i) s a -> Par (Ef p g f b i) s a
addG (WrapPar p) = WrapPar p

addF :: Par (Ef p g NF b i) s a -> Par (Ef p g f b i) s a
addF (WrapPar p) = WrapPar p

addB :: Par (Ef p g f NB i) s a -> Par (Ef p g f b i) s a
addB (WrapPar p) = WrapPar p

addI :: Par (Ef p g f b NI) s a -> Par (Ef p g f b i) s a
addI (WrapPar p) = WrapPar p

