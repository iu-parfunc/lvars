{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DataKinds #-}  -- For Determinism

module Control.LVish.Internal
  (
    Par(..), LVar(..),
    unWrapPar, unsafeRunPar,
    Determinism(..), 
    unsafeConvert, state,
    liftIO,
    for_
  )
  where

import           Control.Monad.IO.Class
import           Control.LVish.MonadToss
import           Control.Applicative
import qualified Control.LVish.SchedIdempotent as L

{-# INLINE state  #-}
{-# INLINE unsafeConvert #-}
{-# INLINE unWrapPar #-}
--------------------------------------------------------------------------------

-- | This datatype is promoted to type-level and used to indicate whether a `Par`
-- computation is guaranteed-deterministic, or only quasi-deterministic (i.e. might throw NonDeterminismExn).
data Determinism = Det | QuasiDet
  deriving Show

-- Use DataKinds promotion to constrain the phantom type argument to be what we want.
newtype Par :: Determinism -> * -> * -> * where
  WrapPar :: L.Par a -> Par d s a
  deriving (Monad, Functor, Applicative)
-- type Foo = Par Det -- This is fine.
-- type Bar = Par Int -- Nice type error for this.

newtype LVar s all delt = WrapLVar { unWrapLVar :: L.LVar all delt }

unWrapPar :: Par d s a -> L.Par a
unWrapPar (WrapPar p) = p 

unsafeRunPar :: Par d s a -> a
unsafeRunPar p = L.runPar (unWrapPar p)

state :: LVar s a d -> a
state = L.state . unWrapLVar

-- | Ignore the extra type annotations regarding both determinism and session-sealing.
unsafeConvert :: Par d1 s1 a -> Par d2 s2 a
unsafeConvert (WrapPar p) = (WrapPar p)

instance MonadIO (Par d s) where
  liftIO = WrapPar . L.liftIO   

instance MonadToss (Par d s) where
  toss = WrapPar L.toss

-- My own forM for numeric ranges (not requiring deforestation optimizations).
-- Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => (Int, Int) -> (Int -> m ()) -> m ()
for_ (start, end) _fn | start > end = error "for_: start is greater than end"
for_ (start, end) fn = loop start
  where
  loop !i | i == end  = return ()
          | otherwise = do fn i; loop (i+1)
  
