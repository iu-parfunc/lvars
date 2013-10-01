module Data.Concurrent.SBag(SBag, new, insert, tryGetAny, initThread) where

import qualified Data.Vector.Mutable as V
import qualified Data.Vector as IV
import GHC.Conc
import Data.Maybe
import Data.IORef
import Data.Atomics
import Data.Atomics.Internal

import Debug.Trace

blockSize :: Int
blockSize = 4

----------------------

data SBag a =
  SBag {
    globalHeadBlock :: V.IOVector (Maybe (Block a)),
    initializedThreads :: V.IOVector Bool,
    tls :: V.IOVector (LocalBag a)
    }

data LocalBag a =
  LocalBag {
    threadBlock :: IORef (Maybe (Block a)),
    stealBlock :: IORef (Maybe (Block a)),
    stealPrev :: IORef (Maybe (Block a)),
    foundAdd :: IORef Bool,
    threadHead :: IORef Int,
    stealHead :: IORef Int,
    stealIndex :: IORef Int
    }

data BlockPtr a =
  BlockPtr { block :: Block a,
             markOne :: Bool,
             markTwo :: Bool } |
  NullBlockPtr

data Block a =
  Block { nodes :: V.IOVector (Maybe a),
          notifyAdd :: V.IOVector Bool,
          next :: BlockPtr a }
          
--  

new :: IO (SBag a)
new = do
  let n = numCapabilities + 1
  ghb <- V.replicate n Nothing
  its <- V.replicate n False
  threads <- V.replicateM n newLocalBag    
  return SBag {
    globalHeadBlock = ghb,
    initializedThreads = its,
    tls = threads
    }
    
newLocalBag :: IO (LocalBag a)
newLocalBag = do
  tb <- newIORef Nothing
  sb <- newIORef Nothing
  sp <- newIORef Nothing
  fa <- newIORef False
  th <- newIORef blockSize
  sh <- newIORef blockSize
  si <- newIORef 0                                              
  return LocalBag {
    threadBlock = tb,
    stealBlock = sb,
    stealPrev = sp,
    foundAdd = fa,
    threadHead = th,
    stealHead = sh,
    stealIndex = si
    }
          
initThread :: SBag a -> IO ()
initThread bag = do
  id <- currentCap
  localData <- V.read (tls bag) id
  globalHeadBlock <- V.read (globalHeadBlock bag) id
  writeIORef (threadBlock localData) globalHeadBlock
  V.write (initializedThreads bag) id True
  
notifyAll :: Maybe (Block a) -> IO ()
notifyAll (Just block) = do
  let na = (notifyAdd block)  
  mapM_ (\i -> V.write na i True) [0..((V.length na) - 1)]
  return ()
      
notifyAll Nothing = return ()

newBlock :: Maybe (Block a) -> IO (Block a)
newBlock block = do
  newNodes <- V.replicate blockSize Nothing
  newNotifyAdd <- V.replicate numCap True
  return Block {
    nodes = newNodes,
    notifyAdd = newNotifyAdd,
    next = buildBlockPtr (block)
    }

buildBlockPtr :: Maybe (Block a) -> BlockPtr a
buildBlockPtr (Just nextBlock) = 
  BlockPtr {   
    block = nextBlock,
    markOne = False,
    markTwo = False
    }
buildBlockPtr Nothing = NullBlockPtr

----------------

insert :: SBag a -> a -> IO ()
insert bag value = do
  id <- currentCap
  localData <- V.read (tls bag) id
    
  let loop :: IO ()
      loop = do
        head <- readIORef (threadHead localData)
        block <- readIORef (threadBlock localData)                  
        if head == blockSize then do
          new_block <- newBlock block              
          V.write (globalHeadBlock bag) id $ Just new_block
          writeIORef (threadBlock localData) $ Just new_block
          writeIORef (threadHead localData) 0
          trace "top" loop
          else do  
          let nodesl = (nodes (fromJust block))
          elem <- V.read nodesl head
          if isNothing elem then do
            notifyAll block
            V.write nodesl head $ Just value
            writeIORef (threadHead localData) (head + 1)
            else do
            writeIORef (threadHead localData) (head + 1)
            trace "bot" loop            
  trace "start" loop
            
-----

tryGetAny :: SBag a -> IO (Maybe a)
tryGetAny bag = do
  id <- currentCap
  localData <- V.read (tls bag) id
  round <- newIORef 0
  let numThreads = numCap
  
  let loop = do
        headd <- readIORef (threadHead localData)
        let head = headd - 1
        block <- readIORef (threadBlock localData)
        putStrLn ("head at start: " ++ (show head))
      
        if isNothing block || (head < 0 && (isNextNull block)) then do
          i <- newIORef 0
          let iLoop = do
                iter <- readIORef i
                if iter >= numThreads then do
                  return Nothing
                  else do
                    r <- readIORef round
                    res <- tryStealBlock bag id r
                    if not $ isNothing res then do
                      return res
                      else do
                        fa <- readIORef (foundAdd localData)
                        if fa then do                                            
                          writeIORef round 0
                          writeIORef i 0
                          iLoop
                          else do
                            fa <- readIORef (foundAdd localData)
                            if fa then do
                              old <- readIORef i
                              writeIORef i (old + 1)
                              iLoop
                            else
                              iLoop
          let rLoop = do
                r <- readIORef round
                let newR = r + 1
                writeIORef round newR
                if newR <= numThreads then do
                  trace "calling rLoop recursively" rLoop
                  else do
                    return Nothing
          trace "calling rLoop" rLoop
          else if head < 0 then do
            writeIORef (threadBlock localData) (Just (nextBlock block))
            writeIORef (threadHead localData) blockSize
            loop
            else do            
              result <- V.read (nodes (fromJust block)) head
              if isNothing result then do 
                writeIORef (threadHead localData) (head - 1)
                trace "reading at head was Nothing" loop
                else do
                  V.write (nodes (fromJust block)) head Nothing
                  writeIORef (threadHead localData) head
                  return result
  trace "main tryGetAny loop" loop
      
tryStealBlock :: SBag a -> Int -> Int -> IO (Maybe a)
tryStealBlock bag id round = do
  localData <- V.read (tls bag) id
  let numThreads = numCap
  head <- readIORef (stealHead localData)
  headRef <- newIORef head
  block <- readIORef (stealBlock localData)
  writeIORef (foundAdd localData) False
  
  ans <- newIORef Nothing
  active <- newIORef True
  
  if isNothing block then do
    index <- readIORef (stealIndex localData)
    block <- V.read (globalHeadBlock bag) index
    writeIORef (stealBlock localData) block
    writeIORef (stealHead localData) 0
    writeIORef headRef 0
    else return ()
  block <- readIORef (stealBlock localData)
  head <- readIORef headRef
  if head == blockSize then do
    nextBlock <- nextStealBlock block
    writeIORef (stealBlock localData) $ Just nextBlock
    writeIORef headRef 0
    else return ()
  block <- readIORef (stealBlock localData)
  head <- readIORef headRef
  if isNothing block then do
    index <- readIORef (stealIndex localData)
    writeIORef (stealIndex localData) $ (index + 1) `mod` numThreads
    writeIORef (stealHead localData) 0
    writeIORef (stealBlock localData) Nothing
    writeIORef (stealPrev localData) Nothing
    writeIORef ans Nothing
    writeIORef active False
    else return ()  
  activeBool <- readIORef active  
  if round == 1 && activeBool then do
    notifyStart block id    
    else do
    nCheck <- notifyCheck block id
    if round > 1 && nCheck then do
      writeIORef (foundAdd localData) True
      else return ()
  let loop lHead = do
        if lHead == blockSize then do
          writeIORef (stealHead localData) head
          writeIORef ans Nothing
          else do
          value <- V.read (nodes $ fromJust block) lHead
          if isNothing value then do
            loop (lHead + 1)
            else do
            -- cas --
            V.write (nodes $ fromJust block) lHead Nothing
            -- end cas --
            writeIORef (stealHead localData) lHead
            writeIORef ans value  
  if activeBool then do loop head else return ()            
  readIORef ans
         
      
nextStealBlock :: Maybe (Block a) -> IO (Block a)
nextStealBlock = undefined

notifyStart :: Maybe (Block a) -> Int -> IO ()
notifyStart = undefined

notifyCheck :: Maybe (Block a) -> Int -> IO Bool
notifyCheck = undefined

isNextNull :: Maybe (Block a) -> Bool 
isNextNull Nothing = True
isNextNull (Just block) = 
  case (next block) of 
    BlockPtr _ _ _ -> False
    NullBlockPtr -> True

nextBlock :: Maybe (Block a) -> Block a
nextBlock Nothing = error "no next block"
nextBlock (Just block) = 
  case (next block) of
    BlockPtr b _ _ -> b
    NullBlockPtr -> error "no next block"
                                              
numCap = numCapabilities

currentCap :: IO Int
currentCap = do
    id <- myThreadId
    res <- threadCapability id
    return $ fst res

-----               
    
--casIOArray :: V.IOVector a -> Int -> (Int, Ticket a)
--casIOArray = undefined

-- demonstrates that the first int is the start index and the second
-- is the number of elements.
foo = do
  arr <- V.replicate 5 True
  let bar (V.MVector a b raw) = (a,b)
  let baz = show $ bar arr
  putStrLn baz    

--------

