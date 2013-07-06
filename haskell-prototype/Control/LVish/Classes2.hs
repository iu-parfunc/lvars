
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
import Control.Monad

import           Data.Hashable
import qualified Data.Set as Set

import qualified Control.LVish as L
import qualified Data.LVar.Set as S


data Determinism = Det | QuasiDet

-- Use DataKinds promotion to constrain the phantom type argument to be what we want.
newtype Par :: Determinism -> * -> * where
  WrapPar :: L.Par a -> Par d a

myRun :: Par Det a -> IO a
myRun = undefined

myQRun :: Par d a -> IO a
myQRun = undefined

unsafeConvert :: Par d1 a -> Par d2 a
unsafeConvert (WrapPar p) = (WrapPar p)

t0 :: Par QuasiDet a
t0 = undefined

-- If you try to "myRun t0" the error is pretty decent:
--
-- <interactive>:90:7:
--     Couldn't match type 'QuasiDet with 'Det
--     Expected type: Par 'Det a0
--       Actual type: Par 'QuasiDet a0
--     In the first argument of `myRun', namely `t0'
--     In the expression: myRun t0
--     In an equation for `it': it = myRun t0
