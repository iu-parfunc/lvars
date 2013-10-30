{-# LANGUAGE Unsafe #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A module for adding the cancellation capability.
-- 
--   In its raw form, this is unsafe, because cancelating could cancel something that
--   would have performed a visible side effect.

module Control.LVish.CancelT where

import Control.Monad.State as S
import Control.Monad.IO.Class 
import Data.IORef

import Control.Par.Class as PC

--------------------------------------------------------------------------------

newtype CancelT m a = CancelT ((StateT CState m) a)
  deriving (Monad, Functor)

-- | Each computation has a boolean flag that stays True while it is still live.
--   Also, the state for one computation is linked to the state of children, so that
--   cancellation may be propagated transitively.
newtype CState = CState (IORef CPair)

instance MonadTrans CancelT where
  lift m = CancelT (lift m)

data CPair = CPair !Bool ![CState]

-- TODO: Replace MonadIO with something the user can't safely access.

poll :: (MonadIO m, LVarSched m) => CancelT m Bool
poll = CancelT$ do
  CState ref  <- S.get
  CPair flg _ <- liftIO$ readIORef ref
  return flg

-- Need some ContT magic here to return to the scheduler...
cancelMe :: CancelT m ()
cancelMe = undefined

instance (MonadIO m, LVarSched m) => LVarSched (CancelT m) where
  type LVar (CancelT m) a d = LVar m a d
--    -- | An alternate form of the monad for quasi-deterministic computations.
--    type QPar m :: * -> *

  type GetSession (CancelT m) = GetSession m

--    forkLV :: m () -> m ()
  forkLV = do
    -- Create new child state..
    undefined
    
--    newLV :: Proxy (m (),a,d) -> IO a -> m (LVar m a d)        
  newLV (_:: Proxy(CancelT m (),a,d)) act =
    lift$ newLV (Proxy::Proxy(m (),a,d)) act
  
--    stateLV :: (LVar m a d) -> (Proxy (m d), a)

  putLV lv putter = do 
     b <- poll
     if b
       then lift $ putLV lv putter
       else cancelMe
     
--    putLV :: LVar m a d             -- ^ the LVar
--          -> (a -> IO (Maybe d))  -- ^ how to do the put, and whether the LVar's
--                                   -- value changed
--          -> m ()
  
  getLV lv globThresh deltThresh =
    -- Poll liveness
    undefined
    
--    getLV :: (LVar m a d)                -- ^ the LVar 
--          -> (a -> Bool -> IO (Maybe b)) -- ^ already past threshold?
--                                         -- The @Bool@ indicates whether the LVar is FROZEN.
--          -> (d ->         IO (Maybe b)) -- ^ does @d@ pass the threshold?
--          -> (Proxy (m(),a,d), m b)

-- --   freezeLV :: LVar m a d -> QPar m ()
--    freezeLV :: LVar m a d -> m (Proxy (m(),a,d))

