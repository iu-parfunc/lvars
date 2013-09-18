module Data.Concurrent.SBag(SBag, new, insert, tryGetAny) where

import Data.Array.IO
import GHC.Conc

blockSize = 4

type Array a = IOUArray Int a

data SBag e = 
  SBag { globalHeadBlock :: Array (Block e),
         numThreads :: Int,
         threadBlock :: Array (Block e),
         stealBlock :: Array (Block e),
         stealPrev :: Array (Block e),
         foundAdd :: Array Bool,
         threadHead :: Array Int,
         stealHead :: Array Int,
         stealIndex :: Array Int
       }
  
data BlockPtr e =
  BlockPtr { block :: Block e, 
             markOne :: Bool, 
             markTwo :: Bool } |
  NullBlockPtr

data Block e = 
  Block { array :: (IOUArray Int e), 
          notifyAdd :: (IOUArray Int Bool),
          blockPtr :: BlockPtr e }

new = 120
insert = 120
tryGetAny = 120
