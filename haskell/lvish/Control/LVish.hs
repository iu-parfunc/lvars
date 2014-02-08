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
    module Control.LVish.EffectSigs,
    liftQD,

    -- * Combinators for manually constraining the type of a given Par computation
    isDet, isQD, isND, 
    hasPut, hasFreeze, hasBump, hasGet, hasIO,
    
    -- * Basic control flow
    fork, yield, 
    --    quiesceAll,    

     -- * Various loop constructs
     parForL, parForSimple, parForTree, parForTiled, for_,

-- This is not fully ready yet till LVish 2.0:
#ifdef GENERIC_PAR
     asyncForEachHP,
#endif

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
     logDbgLn, runParLogged, runParDetailed,
     OutDest(..), DbgCfg (..),
     LVar()
   ) where

-- NOTE : This is an aggregation module:
import           Control.LVish.Types
import           Control.LVish.Internal as I
import           Control.LVish.Basics as B
import           Control.LVish.Logical
import qualified Control.LVish.SchedIdempotent as L
import           Control.LVish.SchedIdempotentInternal (State)
import           Control.LVish.EffectSigs
import           Control.LVish.Logging (OutDest(..))
import           Data.LVar.IVar 

import Data.IORef
--------------------------------------------------------------------------------


#ifdef GENERIC_PAR
import qualified Control.Par.Class as PC
import qualified Control.Par.Class.Unsafe as PU

instance (Deterministic e1, e2 ~ SetF F e1) => 
         PC.ParQuasi (Par e1 s) (Par e2 s) where
  -- WARNING: this will no longer be safe when FULL nondetermiism is possible:
  toQPar act = unsafeConvert act
  
instance PC.ParSealed (Par d s) where
  type GetSession (Par d s) = s
  
instance PC.LVarSched (Par d s) where
  type LVar (Par d s) = L.LVar 

  forkLV = fork
  newLV  = WrapPar . L.newLV
  getLV lv glob delt = WrapPar $ L.getLV lv glob delt
  putLV lv putter    = WrapPar $ L.putLV lv putter

  stateLV (L.LVar{L.state=s}) = (PC.Proxy,s)

  returnToSched = WrapPar $ mkPar $ \_k -> L.sched


instance (Deterministic e1, e2 ~ SetF F e1) => 
         PC.LVarSchedQ (Par e1 s) (Par e2 s)  where
--  freezeLV = WrapPar . L.freezeLV  -- FINISHME

instance PU.ParThreadSafe (Par d s) where
  unsafeParIO = I.liftIO

#endif

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

isDet :: (e ~ (Ef P G NF B NI)) => Par e s a -> Par e s a
isDet x = x

isQD :: (e ~ (Ef P G F B NI)) => Par e s a -> Par e s a
isQD x = x

isND :: (e ~ (Ef P G F B I)) => Par e s a -> Par e s a
isND x = x

