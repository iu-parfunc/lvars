{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies #-}

-- | Not exported directly.  Reexported by "Control.LVish".
module Control.LVish.Internal.Logical (asyncAnd, asyncOr, andMap, orMap) where

import Control.Par.EffectSigs
import Control.LVish.Internal.Basics
import Control.LVish.Internal (Par(WrapPar), unsafeDet)
import Control.LVish.Internal.SchedIdempotent (liftIO, HandlerPool)
import Data.LVar.IVar    as IV

import qualified Data.Atomics.Counter as C

--------------------------------------------------------------------------------

-- | A parallel @And@ operation that can return early---whenever a False appears on either branch.
asyncAnd :: Maybe HandlerPool -> (Par e s Bool) -> (Par e s Bool) -> (Bool -> Par e s ()) -> Par e s ()
asyncAnd hp leftM rightM kont = do
  -- Atomic counter, if we are the second True we write the result:
  cnt <- io$ C.newCounter 0 -- TODO we could share this for 3+-way and.
  let launch m = forkHP hp $
                   do b <- m
                      case b of
                        True  -> do n <- io$ C.incrCounter 1 cnt
                                    if n==2
                                      then kont True
                                      else return ()
                        False -> -- We COULD assume idempotency and execute kont False twice,
                                 -- but since we have the counter anyway let us dedup:
                                 do n <- io$ C.incrCounter 100 cnt
                                    if n < 200 -- Zero ops or one True.
                                      then kont False
                                      else return ()
  launch leftM
  launch rightM
  return ()

-- OR this could expose:
-- asyncAnd :: Maybe HandlerPool -> (Par e s Bool) -> (Par e s Bool) -> Par e s Bool


-- <DUPLICATED CODE>
-- I think this is one of those situations where (efficiently) abstracting is more
-- complicated than permitting a code clone.

-- | Analagous operation for @Or@.
asyncOr :: Maybe HandlerPool -> (Par e s Bool) -> (Par e s Bool) -> (Bool -> Par e s ()) -> Par e s ()
asyncOr hp leftM rightM kont = do
  -- Atomic counter, if we`re the second True we write the result:
  cnt <- io$ C.newCounter 0 -- TODO we could share this for 3+-way and.
  let launch m = forkHP hp $
                   do b <- m
                      case b of
                        False  -> do n <- io$ C.incrCounter 1 cnt
                                     if n==2
                                      then kont False
                                      else return ()
                        True  -> -- We COULD assume idempotency and execute kont False twice,
                                 -- but since we have the counter anyway let`s dedup:
                                 do n <- io$ C.incrCounter 100 cnt
                                    if n < 200 -- Zero ops or one True.
                                      then kont True
                                      else return ()
  launch leftM
  launch rightM
  return ()
-- </DUPLICATED CODE>

--------------------------------------------------------------------------------
-- Lift them to lists:
--------------------------------------------------------------------------------

{-# INLINE andMap #-}
andMap :: (HasGet e, HasPut e) => Maybe HandlerPool -> (a -> Par e s Bool) -> [a] -> Par e s Bool       
andMap = makeMapper asyncAnd

{-# INLINE orMap #-}
orMap :: (HasGet e, HasPut e) => Maybe HandlerPool -> (a -> Par e s Bool) -> [a] -> Par e s Bool       
orMap = makeMapper asyncOr

{-# INLINE makeMapper #-}
makeMapper :: (HasGet e, HasPut e) => 
              (Maybe HandlerPool -> (Par e s Bool) -> (Par e s Bool) -> (Bool -> Par e s ()) -> Par e s ()) ->
              Maybe HandlerPool -> (a -> Par e s Bool) -> [a] -> Par e s Bool       
makeMapper asyncOp hp fn ls = aloop ls 
  where
   aloop []  = return True
   aloop [x] = fn x
   aloop ls2 = do let (x,y) = fastChop ls2
                  tmp <- IV.new -- A place for the intermediate result
                  asyncOp hp (aloop x) (aloop y) (IV.put tmp) -- writeIt
                  IV.get tmp -- IM.getKey thekey table



--------------------------------------------------------------------------------
-- Utilities:
--------------------------------------------------------------------------------

fastChop :: [a] -> ([a],[a])
fastChop ls = loop [] ls ls
 where
   loop !acc !rst1 !rst2 =
     case rst2 of
       []  -> (acc,rst1)
       [_] -> (acc,rst1)
       -- Each time around we chop one from rst1 and two from rst2:
       _:_:rst2' -> let (hd:rst1') = rst1 in
                    loop (hd:acc) rst1' rst2'


io :: IO a -> Par e s a
io = WrapPar . liftIO

