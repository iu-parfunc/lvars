{-# LANGUAGE DeriveDataTypeable #-}

-- | A simple internal module to factor out types that are used in many places.
module Control.LVish.Types
       ( LVishException(..)
       , OutDest (..)
       , DbgCfg(..))
     where

import Data.Typeable (Typeable)
import Control.Exception
import System.IO (Handle)

-- | All @LVar@s share a common notion of exceptions.
--   The two common forms of exception currently are conflicting-put and put-after-freeze.
--   There are also errors that correspond to particular invariants for particular LVars.
data LVishException = ConflictingPutExn String
                    | PutAfterFreezeExn String
                    | LVarSpecificExn   String
  deriving (Show, Read, Eq, Ord, Typeable)

instance Exception LVishException 

-- | A destination for log messages
data OutDest = -- NoOutput -- ^ Drop them entirely.
               OutputEvents    -- ^ Output via GHC's `traceEvent` runtime events.
             | OutputTo Handle -- ^ Printed human-readable output to a handle.
             | OutputInMemory  -- ^ Accumulate output in memory and flush when appropriate.

-- DebugConfig
data DbgCfg = 
     DbgCfg { dbgRange :: Maybe (Int,Int) 
                -- ^ Inclusive range of debug messages to accept
                --   (i.e. filter on priority level).  If Nothing, use the default level,
                --   which is (0,N) where N is controlled by the DEBUG environment variable.
            , dbgDests :: [OutDest] -- ^ Destinations for debug log messages.
            , dbgScheduling :: Bool
                -- ^ In additional to logging debug messages, control
                --   thread interleaving at these points when this is True.
           }
