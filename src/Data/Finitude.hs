{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Data.Finitude
  ( Finitude(..)
  , Union
  , Isect
  ) where

-- | A 'Finitude' value denotes whether something is finite ('F') or
--   infinite ('I').  It exists mostly to be used as a lifted type
--   index.
data Finitude =
    F    -- ^ Finite
  | I    -- ^ Infinite

-- | Union on finitudes. The union of an infinite thing with anything
--   is infinite; the union of two finite things is finite.
--
--   Finitudes form a monoid under this operation with 'F' as the
--   identity element.
type family Union (f1 :: Finitude) (f2 :: Finitude) :: Finitude where
  Union F f = f
  Union I f = I

-- | Intersection on type-level finitudes.  This is not quite as
--   straightforward as union: the intersection of a finite set with
--   anything is finite, but we can't say anything in particular about
--   the size of the intersection of two infinite sets.  However, we
--   think of the finitudes as representing not arbitrary sets but
--   potentially right-infinite intervals on the positive real line.
--   In that case the intersection of two right-infinite intervals
--   must be infinite.
--
--   This operation is associative; 'Finitude' forms a monoid under
--   'Isect' with 'I' as the identity element.
type family Isect (f1 :: Finitude) (f2 :: Finitude) :: Finitude where
  Isect f f = f
  Isect F f = F
  Isect f F = F
  Isect f g = I

  -- or  Isect I g = g  ?


