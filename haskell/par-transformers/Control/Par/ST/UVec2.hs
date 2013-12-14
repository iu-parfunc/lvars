{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

-- | A convenience interface -- simply a restriction of `ParST` to the case
--   of a single, boxed vector as the mutable state.
--
--   This library exposes simple versions of common operations from
--   "Data.Vector.Mutable", which operate directly on the implicit vector state
--   threaded through the monad.

module Control.Par.ST.UVec2
       ( -- * A type alias for parallel computations with @Vector@ state
         ParVec2T, 
         runParVec2T,

         -- * Reexported from the generic interface
         forkSTSplit, liftPar, liftST, 
       
         -- * Retrieving an explict pointer to the Vector
         reify, 
         
         -- * Useful vector helpers
         writeL, writeR, readL, readR, lengthL, lengthR,
         swapL, swapR, dropL, dropR, takeL, takeR,
         growL, growR, setL, setR, swapState
       )
       where

import Control.Par.ST
import Control.Par.Class.Unsafe (ParThreadSafe(unsafeParIO))
import qualified Control.Monad.State.Strict as S
import qualified Data.Vector.Unboxed.Mutable as MU
import Prelude hiding (read, length, drop, take)

#define CONSTRAINT  MU.Unbox
#define FLIPTY UVectorFlp
#define FLPIT UFlp

--------------------------------------------------------------------------------

#include "Vec2Common.hs"
