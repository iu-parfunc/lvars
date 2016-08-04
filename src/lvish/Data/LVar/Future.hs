
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash             #-}

        -- |

module Data.LVar.Future where

import           Control.DeepSeq
import           Control.Exception                      (throw)
import qualified Control.LVish.Basics                   as LV
import           Control.LVish.DeepFrz.Internal
import           Control.LVish.Internal                 (LVar (WrapLVar),
                                                         Par (WrapPar))
import qualified Control.LVish.Internal                 as I
import qualified Control.LVish.Types                    as LV
import           Control.Par.EffectSigs
import qualified Data.Foldable                          as F
import           Data.IORef
import           Data.LVar.Generic
import           Data.LVar.Generic.Internal             (unsafeCoerceLVar)
import           GHC.Prim                               (unsafeCoerce#)
import           Internal.Control.LVish.SchedIdempotent (freezeLV, getLV, newLV,
                                                         putLV)
import qualified Internal.Control.LVish.SchedIdempotent as LI
import           System.IO.Unsafe                       (unsafeDupablePerformIO,
                                                         unsafePerformIO)
import           System.Mem.StableName                  (hashStableName,
                                                         makeStableName)

import qualified Control.Par.Class        as PC
import qualified Control.Par.Class.Unsafe as PU

--------------------------------------------------------------------------------

data FutStatus a = Empty
                 | Started
                 | Finished a

newtype Future s a = Future (LVar s (IORef (FutStatus a)) a)

-- instance (IdempotentParMonad (p e)) => ParFuture Par where

get :: Future s a -> Par e s a
get (IVar (WrapLVar iv)) = WrapPar$ getLV iv globalThresh deltaThresh
  where globalThresh ref _ = readIORef ref    -- past threshold iff Just _
        deltaThresh  x     = return $ Just x  -- always past threshold

instance PC.ParFuture Par where
  type Future Par = Future

  spawn_ m = do ref <- PU.internalLiftIO $ newIORef $ error "Data.LVar.Future - internal error"
                fork (do res <- m
                         PU.internalLiftIO $ do writeIORef ref
                                                -- Mem barrier here
                         )
                undefined
  read = undefined
