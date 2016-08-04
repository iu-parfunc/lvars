{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

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

         -- * Installing new vectors
         installL, installR,

         -- * Common vector operations
         writeL, writeR, readL, readR, lengthL, lengthR,
         swapL, swapR, dropL, dropR, takeL, takeR,
         growL, growR, setL, setR, swapState
       )
       where

import           Control.Par.Class.Unsafe   (ParThreadSafe)
import           Control.Par.ST hiding (reify)

import qualified Control.Monad.Reader as R
-- import qualified Control.Monad.State.Strict as S
import qualified Data.Vector.Mutable        as MU
import           Prelude                    hiding (drop, length, read, take)

#define CONSTRAINT(e)
#define FLIPTY MVectorFlp
#define FLPIT VFlp
#define ARRRECIP MVectorFlpRecipe

--------------------------------------------------------------------------------

-- #include "./Control/Par/ST/Vec2Common.hs"
#include "Vec2Common.hs"
