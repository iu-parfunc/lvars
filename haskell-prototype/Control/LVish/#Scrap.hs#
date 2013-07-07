
#if 0
-- This gets ugly real fast:

class LVarData0 t where
  -- | This associated type models a picture of the "complete" contents of the data:
  -- e.g. a whole set instead of one element, or the full/empty information for an
  -- IVar, instead of just the payload.
  type Snapshot0 t
  freeze0 :: t -> Par (Snapshot0 t)
  newBottom0 :: Par t

instance (LVarData1 f, LVarData0 a) => LVarData0 (f a) where  
  type Snapshot0 (f a) = Snapshot f (Snapshot0 a)
  freeze0 = undefined
  newBottom0 = undefined
#endif



-- instance (LVarData1 f, LVarData1 g, Traversable g) => LVarData1 (g :. f) where
--   -- type Snapshot (g :. f) a = Snapshot g (Snapshot f a)
--   data Snapshot (g :. f) a = ComposedSnap !(Snapshot g (Snapshot f a))
--   freeze (inp :: (g :. f) a) =
--     do let inp' :: g (f a)
--            inp' = unO inp
--        a <- freeze inp' :: Par (Snapshot g (f a))
--        b <- traverseSnap freeze (a :: Snapshot g (f a))
--        return $ ComposedSnap (b :: Snapshot g (Snapshot f a))
--   -- Because newBottom creates an empty structure, there should be no extra work to
--   -- do here.
--   newBottom = newBottom
--     -- let new :: Par (g a)
--     --     new = newBottom
--     -- in new


class DeepFreeze (from :: *) (to :: *) where
  type DeepSnap 
  deepFreeze :: 

#if 0
class LVarData0 (t :: *) where
  -- | This associated type models a picture of the "complete" contents of the data:
  -- e.g. a whole set instead of one element, or the full/empty information for an
  -- IVar, instead of just the payload.
  type Snapshot0 t
  freeze0 :: t -> Par (Snapshot0 t)
  newBottom0 :: Par t

instance (LVarData1 f, LVarData0 a) => LVarData0 (f a) where  
  type Snapshot0 (f a) = Snapshot f (Snapshot0 a)
  freeze0 = undefined
  newBottom0 = undefined
#endif

