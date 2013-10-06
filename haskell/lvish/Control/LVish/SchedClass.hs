{-# LANGUAGE DataKinds, KindSignatures, FlexibleInstances #-}
{-# LANGUAGE CPP, NamedFieldPuns #-}


module Control.LVish.SchedClass where

import Control.Monad
import Control.LVish.SchedIdempotent
-- import Control.LVish.SchedIdempotentInternal
import qualified Control.LVish.SchedIdempotentInternal as Sched
import Data.IORef
import Data.Atomics

import qualified Data.Concurrent.Bag as B

--------------------------------------------------------------------------------

data Idempotency = Idemp | NonIdemp
  deriving Show

newtype Par2 (idp :: Idempotency) a = Par2 (Par a)

-- | This exists solely to choose between idempotent and non-idempotent
-- configurations of the `Par` monad.
class WorkSched par where 
  getLV :: (LVar a d)                  -- ^ the LVar 
        -> (a -> Bool -> IO (Maybe b)) -- ^ already past threshold?
        -> (d ->         IO (Maybe b)) -- ^ does @d@ pass the threshold?
        -> par b

 -- TODO: figure out what to do with addHandler:
 {-
  addHandler :: Maybe HandlerPool           -- ^ pool to enroll in, if any
             -> LVar a d                    -- ^ LVar to listen to
             -> (a -> IO (Maybe (Par ())))  -- ^ initial callback
             -> (d -> IO (Maybe (Par ())))  -- ^ subsequent callbacks: updates
             -> par ()
 -}

instance WorkSched (Par2 Idemp) where
  getLV = mkGetLV Idemp

instance WorkSched (Par2 NonIdemp) where  
  getLV = mkGetLV NonIdemp


{-# INLINE mkGetLV #-}
mkGetLV :: Idempotency -> LVar a1 d
                       -> (a1 -> Bool -> IO (Maybe a))
                       -> (d -> IO (Maybe a))
                       -> Par2 idp a
mkGetLV mode lv@(LVar {state, status}) globalThresh deltaThresh = Par2$
   mkPar $ \k q -> do
    -- tradeoff: we fastpath the case where the LVar is already beyond the
    -- threshhold by polling *before* enrolling the callback.  The price is
    -- that, if we are not currently above the threshhold, we will have to poll
    -- /again/ after enrolling the callback.  This race may also result in the
    -- continuation being executed twice, which is permitted by idempotence.

    curStatus <- readIORef status
    case curStatus of
      Frozen -> do 
        tripped <- globalThresh state True
        case tripped of
          Just b -> exec (k b) q -- already past the threshold; invoke the
                                 -- continuation immediately                    
          Nothing -> sched q     
      Active listeners -> do
        tripped <- globalThresh state False
        case tripped of
          Just b -> exec (k b) q -- already past the threshold; invoke the
                                 -- continuation immediately        

          Nothing -> do          -- /transiently/ not past the threshhold; block        
            let unblockWhen1 thresh tok q = do
                  tripped <- thresh
                  whenJust tripped $ \b -> do        
                    B.remove tok
                    Sched.pushWork q (k b)
                -- Non-idempotent version.  A small amount of duplicated code here:
                unblockWhen2 execFlag thresh tok q = do
                  tripped <- thresh
                  whenJust tripped $ \b -> do        
                    B.remove tok
                    ticket <- readForCAS execFlag
                    unless (peekTicket ticket) $ do
                      (winner, _) <- casIORef execFlag ticket True
                      when winner $ Sched.pushWork q (k b) 
                    
            mflg <- case mode of
                     Idemp    -> return (error "getLV: This value should be unused")
                     NonIdemp -> newIORef False
                    
            let unblock = case mode of
                           Idemp    -> unblockWhen1 
                           NonIdemp -> unblockWhen2 mflg
                onUpdate d = unblock $ deltaThresh d
                onFreeze   = unblock $ globalThresh state True


            -- add listener, i.e., move the continuation to the waiting bag
            tok <- B.put listeners $ Listener onUpdate onFreeze

            -- but there's a race: the threshold might be passed (or the LVar
            -- frozen) between our check and the enrollment as a listener, so we
            -- must poll again
            frozen <- isFrozen lv
            tripped' <- globalThresh state frozen
            case tripped' of
              Just b -> do
                B.remove tok  -- remove the listener we just added, and
                exec (k b) q  -- execute the continuation. this work might be
                              -- redundant, but by idempotence that's OK
              Nothing -> sched q



-- getLV :: (LVar a d)                  -- ^ the LVar 
--       -> (a -> Bool -> IO (Maybe b)) -- ^ already past threshold?
--       -> (d ->         IO (Maybe b)) -- ^ does @d@ pass the threshold?
--       -> Par b

-- getLV lv@(LVar {state, status}) globalThresh deltaThresh = mkPar $ \k q -> do
--   -- tradeoff: we fastpath the case where the LVar is already beyond the
--   -- threshhold by polling *before* enrolling the callback.  The price is
--   -- that, if we are not currently above the threshhold, we will have to poll
--   -- /again/ after enrolling the callback.  This race may also result in the
--   -- continuation being executed twice, which is permitted by idempotence.

--   curStatus <- readIORef status
--   case curStatus of
--     Frozen -> do 
--       tripped <- globalThresh state True
--       case tripped of
--         Just b -> exec (k b) q -- already past the threshold; invoke the
--                                -- continuation immediately                    
--         Nothing -> sched q     
--     Active listeners -> do
--       tripped <- globalThresh state False
--       case tripped of
--         Just b -> exec (k b) q -- already past the threshold; invoke the
--                                -- continuation immediately        

--         Nothing -> do          -- /transiently/ not past the threshhold; block        
          
-- #if GET_ONCE
--           execFlag <- newIORef False
-- #endif
  
--           let onUpdate d = unblockWhen $ deltaThresh d
--               onFreeze   = unblockWhen $ globalThresh state True
              
--               unblockWhen thresh tok q = do
--                 tripped <- thresh
--                 whenJust tripped $ \b -> do        
--                   B.remove tok
-- #if GET_ONCE
--                   ticket <- readForCAS execFlag
--                   unless (peekTicket ticket) $ do
--                     (winner, _) <- casIORef execFlag ticket True
--                     when winner $ Sched.pushWork q (k b) 
-- #else 
--                   Sched.pushWork q (k b)                     
-- #endif
          
--           -- add listener, i.e., move the continuation to the waiting bag
--           tok <- B.put listeners $ Listener onUpdate onFreeze

--           -- but there's a race: the threshold might be passed (or the LVar
--           -- frozen) between our check and the enrollment as a listener, so we
--           -- must poll again
--           frozen <- isFrozen lv
--           tripped' <- globalThresh state frozen
--           case tripped' of
--             Just b -> do
--               B.remove tok  -- remove the listener we just added, and
--               exec (k b) q  -- execute the continuation. this work might be
--                             -- redundant, but by idempotence that's OK
--             Nothing -> sched q

isFrozen :: LVar a d -> IO Bool
isFrozen (LVar {status}) = do
  curStatus <- readIORef status
  case curStatus of
    Active _ -> return False
    Frozen   -> return True

-- mkPar :: ((a -> ClosedPar) -> SchedState -> IO ()) -> Par a
-- mkPar f = Par $ \k -> ClosedPar $ \q -> f k q

whenJust :: Maybe a -> (a -> IO ()) -> IO ()
whenJust Nothing  _ = return ()
whenJust (Just a) f = f a
