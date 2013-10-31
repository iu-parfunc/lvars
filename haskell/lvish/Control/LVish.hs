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
{-# LANGUAGE CPP #-}

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
    Determinism(..), liftQD,
    LVishException(..),
    
    -- * Basic control flow
    fork,
    yield, 
    runPar, runParIO,
--    runParIO_, runParLogged,
--    quiesceAll,    
    
    -- * Various loop constructs
    parForL, parForSimple, parForTree, parForTiled, for_,

    -- * Logical control flow operators
    module Control.LVish.Logical,
    -- asyncAnd, asyncOr, andMap, orMap,
    
    -- * Synchronizing with handler pools
    L.HandlerPool(),    
    newPool, 
    withNewPool, withNewPool_, 
    quiesce, 
    
    forkHP,
    
    -- * Debug facilities and internal bits
    logDbgLn, logDbgLn_, runParLogged, 
    LVar()
  ) where

-- NOTE : This is an aggregation module:
import           Control.LVish.Types
import           Control.LVish.Internal
import           Control.LVish.Basics as B
import           Control.LVish.Logical
import qualified Control.LVish.SchedIdempotent as L
import           Control.LVish.SchedIdempotentInternal (State)

import Data.IORef
--------------------------------------------------------------------------------

#ifdef GENERIC_PAR
import qualified Control.Par.Class as PC

instance PC.ParQuasi (Par d s) where
  type QPar (Par d s) = Par 'QuasiDet s
  
instance PC.ParSealed (Par d s) where
  type GetSession (Par d s) = s
  
instance PC.LVarSched (Par d s) where
  type LVar (Par d s) = L.LVar 

  forkLV = fork
  newLV  = WrapPar . L.newLV
  getLV lv glob delt = WrapPar $ L.getLV lv glob delt
  putLV lv putter    = WrapPar $ L.putLV lv putter

  stateLV (L.LVar{L.state=s}) = (PC.Proxy,s)

  freezeLV = WrapPar . L.freezeLV

  returnToSched = WrapPar $ mkPar $ \_k -> L.sched 
#endif

------ DUPLICATED: -----
mkPar :: ((a -> L.ClosedPar) -> SchedState -> IO ()) -> L.Par a
mkPar f = L.Par $ \k -> L.ClosedPar $ \q -> f k q
type SchedState = State L.ClosedPar LVarID
type LVarID = IORef ()
