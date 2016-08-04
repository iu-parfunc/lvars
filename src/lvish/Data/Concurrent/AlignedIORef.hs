{-# LANGUAGE NamedFieldPuns #-}

-- | Cacheline-aligned wrappers around IORefs.  Currently doing nothing.

module Data.Concurrent.AlignedIORef (AlignedIORef(), newAlignedIORef, ref)
where
  
import Data.IORef  

data AlignedIORef a = AlignedIORef {
  -- pad out to 64 bytes to avoid false sharing (assuming 4 byte words and 64
  -- byte cachelines)
  --  padding :: [IORef a], 
  ref :: {-# UNPACK #-} !(IORef a)
}

newAlignedIORef :: a -> IO (AlignedIORef a)
newAlignedIORef v = do
  ref <- newIORef v
--  padding <- replicateM 15 $ newIORef v
  return $! AlignedIORef {
--    padding,
    ref
  }
