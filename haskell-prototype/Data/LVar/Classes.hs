{-# LANGUAGE TypeFamilies #-}

-- We should be able to abstract certain common features of certain LVar data
-- structures.

module Data.LVar.Classes
       where 

import Control.LVish
import Data.LVar.IVar as I
import Data.LVar.Set  as S
import Data.Set       as Set
--------------------------------------------------------------------------------

-- TODO: Classes for standard datatypes: ivars, sets, other containers:
-- ....

--------------------------------------------------------------------------------

class LVarData1 f where
  -- | This associated type models a picture of the "complete" contents of the data:
  -- e.g. a whole set instead of one element, or the full/empty information for an
  -- IVar, instead of just the payload.
  type Snapshot f a 
  freeze :: f a -> Par (Snapshot f a)
  newBottom :: Par (f a)

  -- What else?
  -- Merge op?


instance LVarData1 IVar where
  type Snapshot IVar a = Maybe a
  freeze = I.freezeIVar
  newBottom = I.new

instance LVarData1 ISet where
  type Snapshot ISet a = Set a
  freeze    = S.freezeSet
  newBottom = S.newEmptySet
