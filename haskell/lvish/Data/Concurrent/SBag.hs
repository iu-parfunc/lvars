module Data.Concurrent.SBag(SBag, new, insert, tryGetAny) where

import qualified Data.Vector.Mutable as V
import qualified Data.Vector as IV
import GHC.Conc
import Data.Maybe

blockSize :: Int
blockSize = 4

type Array a = V.IOVector (Maybe a)

data SBag e = 
  SBag { globalHeadBlock :: Array (Block e),
         numThreads :: Int,
         initializedThreads :: (V.IOVector Bool),
         threadBlock :: Array (Block e),
         stealBlock :: Array (Block e),
         stealPrev :: Array (Block e),
         foundAdd :: Array Bool,
         threadHead :: V.IOVector Int,
         stealHead :: V.IOVector Int,
         stealIndex :: V.IOVector Int
       }
  
data BlockPtr e =
  BlockPtr { block :: Block e, 
             markOne :: Bool, 
             markTwo :: Bool } |
  NullBlockPtr

data Block e = 
  Block { array :: V.IOVector (Maybe e), 
          notifyAdd :: Array Bool,
          blockPtr :: BlockPtr e }

new :: IO (SBag a)
new = do
  let n = numCapabilities
  globalHeadBlock <- V.replicate n Nothing
  initializedThreads <- V.replicate n False
  threadBlock <- V.replicate n Nothing
  stealBlock <- V.replicate n Nothing
  stealPrev <- V.replicate n Nothing
  foundAdd <- V.replicate n Nothing
  threadHead <- V.replicate n blockSize
  stealHead <- V.replicate n blockSize
  stealIndex <- V.replicate n 0
  return SBag {
    globalHeadBlock = globalHeadBlock,
    numThreads = n,
    initializedThreads = initializedThreads,
    threadBlock = threadBlock,
    stealBlock = stealBlock,
    stealPrev = stealPrev,
    foundAdd = foundAdd,
    threadHead = threadHead,
    stealHead = stealHead,
    stealIndex = stealIndex }

initThread :: SBag a -> IO () 
initThread bag = do
  id <- currentCap
  hb <- V.read (globalHeadBlock bag) id
  V.write (threadBlock bag) id hb
--  V.write (threadHead bag) id blockSize
--  V.write (stealIndex bag) id 0
--  V.write (stealHead bag) id blockSize
  V.write (initializedThreads bag) id True
  
insert :: SBag a -> a -> IO ()
insert bag value = do
  id <- currentCap
  threadHead <- V.read (threadHead bag) id 
  head <- return $ threadHead - 1
  blockOpt <- V.read (threadBlock bag) id
  let loop = (\x -> do
               headOpt <- (case blockOpt of
                               Nothing -> do 
                                 return Nothing
                               Just block -> do
                                 res <- V.read (array block) head
                                 return res
                          )
               if (head == blockSize)
               then
                 do
                   return 120
--                 else if (V.read (array block) head) == Nothing then do
               else 
                 do
                   if (isNothing headOpt)
                     then
                       do 
                         return 5
                     else 
                       do
                         return 120)
  return ()
                         
tryGetAny = 120

numCap = numCapabilities

currentCap :: IO Int
currentCap = do 
  id <- myThreadId
  res <- threadCapability id
  return $ fst res

