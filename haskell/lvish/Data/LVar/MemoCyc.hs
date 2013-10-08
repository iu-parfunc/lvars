{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE KindSignatures, EmptyDataDecls #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}

{-|

In contrast with "Data.LVar.Memo", this module provides..............

 -}

module Data.LVar.MemoCyc
       (
         -- * Memo tables and defered lookups 
         Memo, MemoFuture, 
         
         -- * Basic operations
         getLazy, getMemo, force,

         -- * Graph reachability 
         getReachable,

         -- * An idiom for fixed point computations
         makeMemoFixedPoint,
         Response(..)   
       )
       where

import Data.Set (Set)
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M

import Control.LVish
import qualified Control.LVish.Internal as LV
import qualified Control.LVish.SchedIdempotent as LI

import Data.IORef
import Data.LVar.PureSet as IS
import Data.LVar.IVar as IV
import qualified Data.Concurrent.SkipListMap as SLM
import qualified Data.Set as S

import qualified Data.LVar.PureMap as IM
-- import qualified Data.LVar.SLMap as IM
-- import qualified Data.LVar.PureSet as S

import Data.Char (ord)
import Data.List (intersperse)
import System.IO.Unsafe
import Debug.Trace

--------------------------------------------------------------------------------
-- Simple atomic Set accumulators
--------------------------------------------------------------------------------

-- | Could use a more scalable structure here... but we need union as well as
-- elementwise insertion.
type SetAcc a = IORef (S.Set a)
newSetAcc :: Par d s (SetAcc a)
newSetAcc = LV.WrapPar $ LI.liftIO $ newIORef S.empty
readSetAcc :: (SetAcc a) -> Par d s (S.Set a)
readSetAcc r = LV.WrapPar $ LI.liftIO $ readIORef r
insertSetAcc :: Ord a => a -> SetAcc a -> Par d s (S.Set a)
insertSetAcc x ref = LV.WrapPar $ LI.liftIO $
                     atomicModifyIORef' ref (\ s -> let ss = S.insert x s in (ss,ss))
unionSetAcc :: Ord a => Set a -> SetAcc a -> Par d s (S.Set a)
unionSetAcc x ref = LV.WrapPar $ LI.liftIO $
                    atomicModifyIORef' ref (\ s -> let ss = S.union x s in (ss,ss))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A Memo-table that stores cached results of executing a `Par` computation.
-- 
--   This, enhanced, version of the Memo-table also is required to track all the keys
--   that are reachable from each key (for cycle-detection).
data Memo (d::Determinism) s k v =
  -- Here we keep both a Ivars of return values, and a set of keys whose computations
  -- have traversed through THIS key.  If we see a cycle there, we can catch it.
  Memo !(IS.ISet s k)
       !(IM.IMap k s (SetAcc k, IVar s v))
         -- ^ Store all the (transitively) reachable keys from this key.

-- | A result from a lookup in a Memo-table, unforced.
--   The two-stage `getLazy`/`force` lookup is useful to separate
--   spawning the work from demanding its result.
newtype MemoFuture (d :: Determinism) s b = MemoFuture (Par d s b)

--------------------------------------------------------------------------------

{-

-- | Reify a function in the `Par` monad as an explicit memoization table.
--   When you build a Memo this way it will not have any cycle-detection capability.
makeMemo :: (Ord a, Eq b) => (a -> Par d s b) -> Par d s (Memo d s a b)
makeMemo fn = do
  st <- newEmptySet
  mp <- IM.newEmptyMap
  IS.forEach st $ \ elm -> do
    res <- fn elm
    IM.insert elm res mp
  return $! Memo st mp
-- TODO: this version may want to have access to the memo-table within the handler as
-- well....
-}


-- | Read from the memo-table.  If the value must be computed, do that right away and
-- block until its complete.
getMemo :: (Ord a, Eq b) => Memo d s a b -> a -> Par d s b 
getMemo tab key =
  do fut <- getLazy tab key
     force fut

-- | Begin to read from the memo-table.  Initiate the computation if the key is not
-- already present.  Don't block on the computation being complete, rather, return a
-- future.
getLazy :: (Ord a, Eq b) => Memo d s a b -> a -> Par d s (MemoFuture d s b)
getLazy (Memo st mp) key = do 
  IS.insert key st
  (_, iv) <- IM.getKey key mp
  return $! MemoFuture (IV.get iv)


getReachable :: (Ord k, Eq v) => Memo d s k v -> k -> Par d s (S.Set k)
getReachable (Memo st mp) key = do 
  IS.insert key st -- Execute it, if it hasn't already.
  (set, iv) <- IM.getKey key mp
  _ <- IV.get iv
  readSetAcc set

-- | This will throw exceptions that were raised during the computation, INCLUDING
-- multiple put.
force :: MemoFuture d s b -> Par d s b 
force (MemoFuture pr) = pr
-- FIXME!!! Where do errors in the memoized function (e.g. multiple put) surface?
-- We must pick a determined, consistent place.
-- 
-- Multiple put errors may not be able to wait until this point to get
-- thrown.  Otherwise we'd have to be at least quasideterministic here.  If you have
-- a MemoFuture you never force, it and an outside computation may be racing to do a
-- put.  If the outside one wins the MemoFuture is the one that gets the exception
-- (and hides it), otherwise the exception is exposed.  Quasideterminism.

-- It may be fair to distinguish between internal problems with the MemoFuture
-- (deferred exceptions), and problematic interactions with the outside world (double
-- put) which would then not be deferred.  Such futures can't be canceled anyway, so
-- there's really no need to defer the exceptions.


--------------------------------------------------------------------------------
-- Cycle-detecting memoized computations
--------------------------------------------------------------------------------

-- | A private type for keeping track of which keys we have gone through to get where
-- we are.
newtype RememberSet key = RememberSet (S.Set key)

--  A Par-monad transformer that adds the cycle-detection capability.
-- 
-- newtype MemoCycT (d::Determinism) s key par res = MemoCycT ()
--
-- UH OH, we need generic version of set, map, and maybe 's' param operations to be
-- able to make this a general transformer I think....

-- | The LVish Par monad extended with the capability to detect cycles in memoized
-- computations.
newtype MemoCycT (d::Determinism) s key res = MemoCycT ()

--------------------------------------------------------------------------------

data Response par key ans =
    Done !ans
  | Request !key (RequestCont par key ans)
    
type RequestCont par key ans = (ans -> par (Response par key ans))

-- | Make a Memo table with the added capability that any cycles in requests will be
-- detected.  A special cycle-handler determines what result is returned when a cycle
-- is detected starting at a given key.
--
-- The result of this function Memo-table returns 
makeMemoFixedPoint :: forall d s k v . (Ord k, Eq v, Show k) =>
                      (k -> Par d s (Response (Par d s) k v)) -- ^ Initial computation to perform for new requests
                   -> (k -> Par d s v)                        -- ^ Handler for a cycle on @k@.
                   -> Par d s (Memo d s k v)
makeMemoFixedPoint initCont cycHndlr = do
  -- The set provides our front-line memoization when new key come.
  set <- IS.newEmptySet
  -- The map stores results:
  mp  <- IM.newEmptyMap
  IS.forEach set $ \ key0 -> do
    key0_vr    <- IV.new
    key0_reach <- newSetAcc
    IM.insert key0 (key0_reach,key0_vr) mp

    -- The accumulator stores continuations waiting for an answer:
    let loop :: S.Set k -> (Response (Par d s) k v) -> Par d s ()
        loop hist resp = do 
         dbg ("![MemoFixedPoint] going around loop, hist length "++show (S.size hist))
         case resp of
           Done ans -> do dbg ("  Final result on key: "++showID key0)
                          -- unionSetAcc hist key0_reach
                          IV.put_ key0_vr ans
           Request key2 newCont -> do
             hist' <- insertSetAcc key2 key0_reach
             dbg ("  Added "++show key2++" to history of "++show key0++" yielding "++show hist')
             if S.member key2 hist then do
               res <- cycHndlr key2
               (key2_reach, key2_resIV) <- IM.getKey key2 mp
               insertSetAcc key2 key2_reach
               dbg ("  HIT CYCLE, key: "++showWID key2)
               IV.put_ key2_resIV res
               dbg (" umm... what to do for key0 after the cycle is found for key2...?")
              else do
               IS.insert key2 set
               (key2_reach, key2_resIV) <- IM.getKey key2 mp               
               let cyc_check elseCase = do
                     reach <- readSetAcc key2_reach 
                     if S.member key0 reach then do
                         dbg ("... found cycle on key0 "++show key0++", between components...") 
                         insertSetAcc key0 key0_reach
                         ans <- cycHndlr key0
                         IV.put_ key0_vr ans
                      else elseCase
                           
               -- PRECHECK: before we block waiting for the other component to finish:
               cyc_check $ do
--               do
                 dbg ("  About to block on intermediate result for key: "++showWID key2)
                 LV.WrapPar$ LI.liftIO$ putStrLn =<< showMapContents mp
                 res <- IV.get key2_resIV
                 dbg ("  |-> DONE blocking on key: "++showWID key2)
-- UNION FINAL CHILD REACHABLE INTO key0 REACHABLE!:
                 keyset <- readSetAcc key2_reach
                 hist'' <- unionSetAcc keyset key0_reach
                 cyc_check $ do -- POSTCHECK: when key2 is finished
                   resp' <- newCont res
                   loop hist'' resp'
    resp <- initCont key0
    loop (S.singleton key0) resp
    
  return $! Memo set mp

showMapContents (IM.IMap lv) = do
  mp <- readIORef (LV.state lv)
  let lst = M.toList mp
  return$ "    Map Contents: (length "++ show (length lst) ++")\n" ++
    concat [ "      "++fullempt++" "++showWID k++" -> "++vals++"\n"
           | (k,(v,IV.IVar ivr)) <- lst
--            , let vals = "hello"
           , let lst = S.toList $ unsafePerformIO (readIORef v)
           , let vals = "#"++show (length lst)++"["++ (concat $ intersperse ", " $ map showID lst)  ++"]"
           , let fullempt = if Nothing == unsafePerformIO (readIORef (LV.state ivr))
                            then "[empty]"
                            else "[full]"
           ]



{-# INLNINE dbg #-}
dbg :: Monad m => String -> m ()
dbg s = trace s (return ())
dbg _ = return ()

showWID :: Show a => a -> String
showWID x = let str = (show x) in
            if length str < 10
            then str
            else showID x++"__"++str

showID :: Show a => a -> String
showID x = let str = (show x) in
           if length str < 10 then str
           else (show (length str))++"-"++ show (checksum str)

checksum str = sum (map ord str)


mem02 :: Bool
mem02 = runPar $ do
  m <- makeMemoFixedPoint (\_ -> return (Request 33 (\_ -> return (Done False))))
                          (\_ -> return True)
  getMemo m 33

mem03 :: (Bool, S.Set Int, S.Set Int)
mem03 = runPar $ do
  m <- makeMemoFixedPoint fn (\_ -> return True)
  bl <- getMemo m 33
  r1 <- getReachable m 33
  r2 <- getReachable m 44
  return (bl, r1, r2)
 where
   fn 33 = return (Request 44 (\_ -> return (Done False)))
   fn 44 = return (Request 33 (\_ -> return (Done False)))

mem04 :: (Bool, S.Set Int, S.Set Int, S.Set Int)
mem04 = runPar $ do
  m <- makeMemoFixedPoint fn (\_ -> return True)
  bl <- getMemo m 33
  r1 <- getReachable m 33
  r2 <- getReachable m 44
  r3 <- getReachable m 55
  return (bl, r1, r2, r3)
 where
   fn 33 = return (Request 44 (\_ -> return (Done False)))
   fn 44 = return (Request 55 (\_ -> return (Done False)))
   fn 55 = return (Request 33 (\_ -> return (Done False)))

{-


-- | This version watches for, and catches, cyclic requests to the memotable that
-- would normally diverge.  Once caught, the user specifies what to do with these
-- cycles by providing a handler.  The handler is called on the key which formed the
-- cycle.  That is, computing the invocation spawned by that key results in a demand
-- for that key.  
makeMemoCyclic :: (MemoTable d s a b -> a -> Par d s b) -> (a -> Par d s b) -> Par d s (MemoTable d s a b)
makeMemoCyclic normalFn ifCycle  = undefined
-- FIXME: Are there races where more than one cycle can be hit?  Can we guarantee
-- that all are hit?  



-- | Cancel an outstanding speculative computation.  This recursively attempts to
-- cancel any downstream computations in this or other memo-tables that are children
-- of the given `MemoFuture`.
cancel :: MemoFuture Det s b -> Par Det s ()
-- FIXME: Det needs to be replaced here with "GetOnly".
cancel fut = undefined

-}
