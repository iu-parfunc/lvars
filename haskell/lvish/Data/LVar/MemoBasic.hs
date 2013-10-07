{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE KindSignatures, EmptyDataDecls #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}

{-|

This basic version of memotables is implemented on top of existing LVars without
breaking any rules.

The problem is that it cannot do cycle detection, because that requires tracking
extra information (where we've been) which is NOT exposed to the user and NOT used 

 -}
module Data.LVar.MemoBasic
       (
         -- * Memo tables and defered lookups 
         Memo, MemoFuture,
         
         -- * Basic operations
         getLazy, getMemo, force,

         -- * An idiom for fixed point computations
         makeMemoFixedPoint,
         Response(..)   

       ) where


import Control.LVish
import qualified Data.Set as S
import qualified Data.LVar.SLMap as IM
import Data.LVar.SLSet as IS
import Data.LVar.IVar as IV

--------------------------------------------------------------------------------
-- Imaginatary memoization interface:

-- | A Memo-table that stores cached results of executing a `Par` computation.
data Memo (d::Determinism) s a b = Memo !(IS.ISet s a) !(IM.IMap a s b)
-- We COULD implement a memo table on top of existing LVars, with a Set LVar whose
-- call-back invokes the function, and then a Map to store the results.  A "get"
-- would become a put on the Set followed by a get on the Map.

-- | A result from a lookup in a Memo-table, unforced.
newtype MemoFuture (d :: Determinism) s b = MemoFuture (Par d s b)

-- | Reify a function in the `Par` monad as an explicit memoization table.
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
  return $! MemoFuture (IM.getKey key mp)


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
-- A fixed-point combinator
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
makeMemoFixedPoint :: (Ord k, Eq v) =>
                      (k -> Par d s (Response (Par d s) k v)) -- ^ Initial computation to perform for new requests
                   -> (k -> Par d s v)                        -- ^ Handler for a cycle on @k@.
                   -> Par d s (Memo d s k v)
makeMemoFixedPoint initCont cycHndlr = do
  -- The set provides our front-line memoization when new key come.
  set <- IS.newEmptySet
  -- The map stores results:
  mp  <- IM.newEmptyMap
  IS.forEach set $ \ key0 -> do
    -- The accumulator stores continuations waiting for an answer:
    let loop hist resp = 
         case resp of
           Done ans -> IM.insert key0 ans mp
           Request key2 newCont
             | S.member key2 hist -> do res <- cycHndlr key2 
                                        IM.insert key2 res mp
             | otherwise -> do 
               IS.insert key2 set
               res   <- IM.getKey key2 mp
               resp' <- newCont res
               loop (S.insert key2 hist) resp'
    resp <- initCont key0
    loop (S.singleton key0) resp
    
  return $! Memo set mp



{-


-- | Cancel an outstanding speculative computation.  This recursively attempts to
-- cancel any downstream computations in this or other memo-tables that are children
-- of the given `MemoFuture`.
cancel :: MemoFuture Det s b -> Par Det s ()
-- FIXME: Det needs to be replaced here with "GetOnly".
cancel fut = undefined

-}
