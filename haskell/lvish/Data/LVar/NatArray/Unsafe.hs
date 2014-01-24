{-# LANGUAGE GADTs #-}

-- | Unsafe operations on NatArray.  NOT for end-user applications.

module Data.LVar.NatArray.Unsafe
  ( NatArray(..), unsafePeek )
  where
import qualified Data.Vector.Storable.Mutable as M
import Foreign.Storable (sizeOf, Storable)
-- import System.IO.Unsafe (unsafeDupablePerformIO)
import           Control.LVish.Internal as LI

------------------------------------------------------------------------------------------

-- | An array of bit-fields with a monotonic OR operation.  This can be used to model
--   a set of Ints by setting the vector entries to zero or one, but it can also
--   model other finite lattices for each index.
-- newtype NatArray s a = NatArray (LVar s (M.IOVector a) (Int,a))
data NatArray s a = Storable a => NatArray !(LVar s (M.IOVector a) (Int,a))

unsafePeek :: (Num a, Eq a) => NatArray s a -> Int -> Par d s (Maybe a)
unsafePeek (NatArray lv) ix = do
  peek <- LI.liftIO $ M.read (LI.state lv) ix
  case peek of
    -- TODO: generalize:
    0 -> return Nothing
    x -> return $! Just x 
