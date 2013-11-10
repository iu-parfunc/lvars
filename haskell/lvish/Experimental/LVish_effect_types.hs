{-# LANGUAGE DataKinds #-}  -- For Determinism
{-# LANGUAGE KindSignatures #-}  -- For Determinism
{-# LANGUAGE GADTs #-}  -- For Determinism
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

import Control.Monad
import Control.Applicative

--------------------------------------------------------------------------------
-- Approach 0:

-- data EffProd = Put | Put_Freeze | Freeze
--              | Put_Bump | Freeze_Bump
--   deriving Show

#if 1
--------------------------------------------------------------------------------
-- Approach 1:

data Determinism = Det | QuasiDet 
  deriving Show

-- type EffProd = (Putting, Freezing, Bumping)
-- data EffProd (a::Putting) (b::Freezing) (c::Bumping) -- = EffProd -- a b c

data EffProd  = MkEffProd Putting Freezing Bumping
data Putting  = Put    | NoPut
data Freezing = Freeze | NoFreeze
data Bumping  = Bump   | NoBump
-- data IOing  = YesIO   | NoIO

-- x :: IsDet d => Par d s Int
-- x = undefined   

-- class IsDet (d :: Determinism) where
-- instance IsDet Det where

-- class IsDet (d :: EffProd a b c) where
class IsDet (d :: EffProd) where
instance IsDet (MkEffProd p NoFreeze b) where 
instance IsDet (MkEffProd NoPut f b) where

class DoesPut (efs :: EffProd) where 
instance DoesPut (MkEffProd Put f ni) where

class DoesFreeze (efs :: EffProd) where 
instance DoesFreeze (MkEffProd p Freeze ni) where 
  
----------------------------------------
  
data LVar s a
  
put :: DoesPut d => LVar s a -> a -> Par d s ()
put = undefined

freeze :: DoesFreeze d => LVar s a -> a -> Par d s ()
freeze = undefined

p1 :: DoesPut d => Par d s ()
p1 = put (undefined::LVar s Int) (3::Int)

p2 :: DoesFreeze d => Par d s ()
p2 = freeze (undefined::LVar s Int) 3

p3 :: (DoesPut d, DoesFreeze d) => Par d s ()
p3 = do p1; p2

p4 :: Par (MkEffProd Put Freeze ni) s ()
p4 = p3

-- p5 :: Par (MkEffProd NoPut Freeze ni) s ()
-- p5 = p4

-- p1 :: Par 
  
-- newtype Par :: Determinism -> * -> * -> * where
newtype Par :: EffProd -> * -> * -> * where
  WrapPar :: Double -> Par d s a

instance Monad (Par efs s) where

-- put :: DoesPut d => LVar s a -> a -> Par d s aa

-- We'd need an indexed monad with a special bind... needs rebindable
-- 'do' syntax.

--------------------------------------------------------------------------------
#else

data Effect = UsesFreeze
            | UsesPut
            | NonIdemp
  deriving Show

newtype Par2 :: [Effect] -> * -> * -> * where
  WrapPar2 :: Double -> Par2 efs s a

y :: Par2 [UsesPut, UsesFreeze] s Int
y = undefined

z :: Par2 [UsesFreeze, UsesPut] s Int
z = undefined

class DoesPut (x :: [Effect]) where

instance DoesPut (UsesPut ': tail) where
instance DoesPut xs => DoesPut (hd ': xs) where
                  
-- instance DoesPut ((:) Effect 'UsesPut tail) where  
-- foo :: DoesPut x => x -> Int
-- foo = undefined
-- _ = foo y
-- _ = foo z 
#endif


main = putStrLn "hi"
