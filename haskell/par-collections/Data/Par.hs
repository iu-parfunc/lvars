
-- | Convenience module that reexports various parallel combinators.

module Data.Par
       (
         -- * Parallle consmuption of /O(1)/-splittable data
         module Data.Par.Splittable,         
         -- * Ranges of numbers, aka iteration spaces
         module Data.Par.Range,
         -- * Basic per-element parallelism for Traversables
         module Data.Par.Traversable
       )
       where


import Data.Par.Traversable
import Data.Par.Splittable 
import Data.Par.Range hiding (pmapReduce, pmapReduce_)
