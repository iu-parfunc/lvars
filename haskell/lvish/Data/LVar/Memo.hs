{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -O2 #-}

{-|

This basic version of memotables is implemented on top of existing LVars without
breaking any rules.

The problem is that it cannot do cycle detection, because that requires tracking
extra information (where we've been) which is NOT exposed to the user and NOT used

 -}
module Data.LVar.Memo
       (
         -- * Memo tables and defered lookups
         Memo, MemoFuture, makeMemo,

         -- * Memo table operations
         getLazy, getMemo, force
       ) where
import Debug.Trace

import           Control.LVish
import qualified Data.LVar.PureMap as IM
import           Data.LVar.PureSet as IS

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A Memo-table that stores cached results of executing a `Par` computation.
data Memo (e::EffectSig) s a b =
     Memo !(IS.ISet s a)
          !(IM.IMap a s b)

-- | A result from a lookup in a Memo-table, unforced.
--   The two-stage `getLazy`/`force` lookup is useful to separate
--   spawning the work from demanding its result.
newtype MemoFuture (e :: EffectSig) s b = MemoFuture (Par e s b)

--------------------------------------------------------------------------------

-- | Reify a function in the `Par` monad as an explicit memoization table.
makeMemo :: (HasPut e, Ord a, Eq b, Show a, Show b) =>
            (a -> Par e s b) -> Par e s (Memo e s a b)
makeMemo fn = do
  st <- newEmptySet
  mp <- IM.newEmptyMap
  IS.forEach st $ \ elm -> do
    res <- fn elm
    trace ("makeMemo, about to insert result: "++show (show elm, show res)) $
      IM.insert elm res mp
  return $! Memo st mp
-- TODO: this version may want to have access to the memo-table within the handler as
-- well....


-- | Read from the memo-table.  If the value must be computed, do that right away and
-- block until its complete.
getMemo :: (HasPut e, HasGet e, Ord a, Eq b) => Memo e s a b -> a -> Par e s b
getMemo tab key =
  do fut <- getLazy tab key
     force fut

-- | Begin to read from the memo-table.  Initiate the computation if the key is not
-- already present.  Don't block on the computation being complete, rather, return a
-- future.
getLazy :: (HasPut e, HasGet e, Ord a, Eq b) => Memo e s a b -> a -> Par e s (MemoFuture e s b)
getLazy (Memo st mp) key = do
  IS.insert key st
  return $! MemoFuture (IM.getKey key mp)


-- | This will throw exceptions that were raised during the computation, INCLUDING
-- multiple put.
force :: MemoFuture e s b -> Par e s b
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



{-


-- | Cancel an outstanding speculative computation.  This recursively attempts to
-- cancel any downstream computations in this or other memo-tables that are children
-- of the given `MemoFuture`.
cancel :: MemoFuture Det s b -> Par Det s ()
-- FIXME: Det needs to be replaced here with "GetOnly".
cancel fut = undefined

-}
