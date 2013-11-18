{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns  #-} 
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

-- | A convenience interface -- simply a restriction of `ParST` to the case
--   of a single, boxed vector as the mutable state.
--
--   This library exposes simple versions of common operations from
--   "Data.Vector.Mutable", which operate directly on the implicit vector state
--   threaded through the monad.

module Control.Par.ST.Vec2
       ( -- * A type alias for parallel computations with @Vector@ state
         ParVec2T, 
         runParVec2T,

         -- * Reexported from the generic interface
         forkSTSplit, liftPar, 
         
         -- * Retrieving an explict pointer to the Vector
         reify, liftST,
         
         -- * Common vector operations
         writeL, writeR, readL, readR, lengthL, lengthR,
         swapL, swapR, dropL, dropR, takeL, takeR, 
         growL, growR, setL, setR, swapState
       )
       where

import Control.Par.ST
import Control.Par.Class.Unsafe (ParThreadSafe(unsafeParIO))

import qualified Control.Monad.State.Strict as S
import qualified Data.Vector.Mutable as MU
import Prelude hiding (read, length, drop, take)

#define CONSTRAINT(e) 
#define FLIPTY MVectorFlp
#define FLPIT VFlp

--------------------------------------------------------------------------------

#include "Vec2Common.hs"

