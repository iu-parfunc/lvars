{-# LANGUAGE TemplateHaskell #-}

module Main (tests, main) where

-- import Control.LVish
-- import qualified Control.Par.Class as PC
import Control.Par.Class.Unsafe (ParMonad(internalLiftIO))
-- import Control.LVish.Unsafe
-- import Data.LVar.IVar as IV

import Data.Maybe (fromMaybe)
import Data.Word

-- import Control.Concurrent
-- import Control.Monad.Trans

import Control.Exception (evaluate)
import Control.Par.Class as PC (Generator(..))
import Data.Par.Range
import Data.IORef
import qualified Control.Monad.Par as P -- This expects monad-par >= 0.3.4.6 with -fnewgeneric
import qualified Data.Atomics.Counter as C

-- import Data.List
import Test.HUnit (Assertion, assert, assertEqual, assertBool, Counts(..))
import Test.Framework.Providers.HUnit
import Test.Framework -- (Test, defaultMain, testGroup)
import Test.Framework.TH (testGroupGenerator)

import TestHelpers (numElems, timeit)

--------------------------------------------------------------------------------

size :: Int
size = fromMaybe 100 numElems

-- Sum from 1..N
expectedSum :: Word64
expectedSum = (s * (s + 1)) `quot` 2
  where s = fromIntegral size

-- About 0.3s for 500M on a laptop:
case_seqfold :: Assertion
case_seqfold = do
  assertEqual "Fold a range of ints" expectedSum =<<
    (timeit $ evaluate $
     fold (\ x y -> x + fromIntegral y) 0 $ irange 1 size)

-- About 0.44s for 500M ints on the same laptop.  That is -- it's slower.
case_seqfoldM :: Assertion
case_seqfoldM = do
  assertEqual "Fold a range of ints in Par" expectedSum =<<
    (timeit $ foldM (\ x y -> return $! x + fromIntegral y) 0 $ irange 1 size)

-- Same thing in a different monad.  This gets about the same time 0.45s.
case_seqfoldMP :: Assertion
case_seqfoldMP = do
  assertEqual "Fold a range of ints in IO" expectedSum =<<
    (timeit $ P.runParIO $
     foldMP (\ x y -> return $! x + fromIntegral y) 0 $ irange 1 size)

-- Runs at 0.3s for 500M if there's no work done in the body at all.  With an IORef
-- Word (i.e. boxed), this is 10X slower, at 0.28s for 50M Switching to
-- Data.Atomic.Counter drops that time by a factor of three, i.e. 0.9s for 500M, or
-- twice as slow as the fold versions above.
--
--   Side note 1: it's 2X slower to use the C.incrCounter atomic op than to use raw
--   reads and writes, but some of this must be due to the fact that the
--   fetch-and-add primop is not an inline primop yet.
--
--   Side note 2: Using a custom vs. default implementatino of forM_ for Ranges
--   didn't make a difference here.
case_seqfor1 :: Assertion
case_seqfor1 = do
  assertEqual "For loop over a range of ints" expectedSum =<<
    (timeit $ do
       cnt <- C.newCounter 0
       PC.forM_ (irange 1 size) $ \ i -> do 
         x <- C.readCounter cnt
         C.writeCounter cnt $! x + fromIntegral i
--         C.incrCounter (fromIntegral i) cnt 
         return ()
       fmap fromIntegral $ C.readCounter cnt 
       -- cnt <- newIORef 0
       -- PC.forM_ (irange 1 size) $ \ i -> do 
       --   x <- readIORef cnt
       --   writeIORef cnt $! x + fromIntegral i
       --   return ()
       -- readIORef cnt        
    )

-- Very slow currently [2013.12.07]: 5M in 0.37s, a full 100X worse.  Providing a
-- custom definition of forMP_ for Ranges (rather than the default) got this down to
-- 0.13s for 5M, but that is still abysmal.
--  
--   Of course, if you perform the internalLiftIO once, outside the loop, the
--   performance is the same as seqfor1.
case_seqforMP1 :: Assertion
case_seqforMP1 = do
  assertEqual "For loop over a range of ints in Par monad" expectedSum =<<
    (timeit $ P.runParIO $ do 
       cnt <- internalLiftIO $ C.newCounter 0
       PC.forMP_ (irange 1 size) $ \ i -> do 
         x <- internalLiftIO$ C.readCounter cnt
         internalLiftIO$ C.writeCounter cnt $! x + fromIntegral i
         return ()
       fmap fromIntegral $ internalLiftIO$ C.readCounter cnt 
    )

-- Do no work in this version, but run the loop in the par monad.
-- This one is reasonable speed, 0.29s for 500M.
case_seqforMP2 :: Assertion
case_seqforMP2 = do
  assertEqual "For loop over a range of ints in Par monad" () =<<
    (timeit $ P.runParIO $ do 
       PC.forMP_ (irange 1 size) $ \ i -> 
         return ()
    )

-- --------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain [tests]

