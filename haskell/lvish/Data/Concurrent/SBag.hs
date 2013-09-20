module Data.Concurrent.SBag(SBag, new, insert, tryGetAny) where

import qualified Data.Vector.Mutable as V
import qualified Data.Vector as IV
import GHC.Conc

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
  Block { array :: Array e, 
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
  threadHead <- V.replicate n Nothing
  stealHead <- V.replicate n Nothing
  stealIndex <- V.replicate n Nothing
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
  V.write (threadHead bag) id (Just blockSize)
  V.write (stealIndex bag) id (Just 0)
  V.write (stealHead bag) id (Just blockSize)
  V.write (initializedThreads bag) id True
  
insert :: SBag a -> a -> IO ()
insert bag value = do
  id <- currentCap
  threadHead <- V.read (threadHead bag) id 
  head <- threadHead - 1
  block <- V.read (threadBlock bag) id
  if head == blockSize {
    let oldBlock = block
        

insert = 120
tryGetAny = 120

numCap = numCapabilities

currentCap :: IO Int
currentCap = do 
  id <- myThreadId
  res <- threadCapability id
  return $ fst res

