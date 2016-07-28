{-# LANGUAGE DeriveDataTypeable #-}

-- | A simple internal module to factor out types that are used in many places.
module Control.LVish.Internal.Types
       ( LVishException(..) )
     where

import Data.Typeable (Typeable)
import Control.Exception

-- | All @LVar@s share a common notion of exceptions.
--   The two common forms of exception currently are conflicting-put and put-after-freeze.
--   There are also errors that correspond to particular invariants for particular LVars.
data LVishException = ConflictingPutExn String
                    | PutAfterFreezeExn String
                    | LVarSpecificExn   String
  deriving (Show, Read, Eq, Ord, Typeable)

instance Exception LVishException 
