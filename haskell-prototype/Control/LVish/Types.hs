
module Control.LVish.Types
       where

------------------------------------------------------------------------------
-- Interface for generic LVar handling
------------------------------------------------------------------------------

-- class Traversable f => LVarData1 (f :: * -> *) where

-- | TODO: if there is a Par class, it needs to be a superclass of this.
class LVarData1 (f :: * -> *) where
  -- | This associated type models a picture of the "complete" contents of the data:
  -- e.g. a whole set instead of one element, or the full/empty information for an
  -- IVar, instead of just the payload.
  -- type Snapshot f a :: *
  data Snapshot f :: * -> *
  
  freeze :: f a -> Par (Snapshot f a)
  newBottom :: Par (f a)

  -- QUESTION: Is there any way to assert that the snapshot is still Traversable?
  -- I don't know of a good way, so instead we expose this:
  traverseSnap :: (a -> Par b) -> Snapshot f a -> Par (Snapshot f b)

  -- What else?
  -- Merge op?

-- -- | This relies on type-level composition of unary type constructors, for which we
-- -- depend on "Control.Compose".
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

-- This gets messy if we try to handle several Kinds:
class LVarData0 (t :: *) where
  -- | This associated type models a picture of the "complete" contents of the data:
  -- e.g. a whole set instead of one element, or the full/empty information for an
  -- IVar, instead of just the payload.
  type Snapshot0 t
  freeze0 :: t -> Par (Snapshot0 t)
  newBottom0 :: Par t

