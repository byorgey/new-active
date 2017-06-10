{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Duration
-- Copyright   :  2017 XXX
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :
--
-- A type for finite and infinite durations.
-----------------------------------------------------------------------------

module Data.Duration
  ( Duration(..)

    -- * Conversion

  , toDuration, fromDuration, fromFDuration

    -- * Operations

  , addDuration, maxDuration, minDuration, compareDuration

  ) where

import           Data.Finitude

import           Linear.Vector

------------------------------------------------------------
-- Durations
------------------------------------------------------------

-- | The type of (potentially infinite) /durations/ over a given
--   numeric type @n@.  The type index @f@ indicates whether the
--   duration is finite or infinite.  The infinite duration is longer
--   than any finite duration.
data Duration f n where

  -- | A finite duration of a given nonnegative length.  The length
  --   could be zero.
  Duration :: n -> Duration F n

  -- | An infinite duration.
  Forever  ::      Duration I n

deriving instance Show n => Show (Duration f n)
deriving instance Functor (Duration f)

instance Applicative (Duration F) where
  pure = Duration
  Duration f <*> Duration x = Duration (f x)

instance Eq n => Eq (Duration f n) where
  Duration n1 == Duration n2 = n1 == n2
  Forever     == Forever     = True

-- | Note that the 'Ord' instance for 'Duration' is not quite as
--   useful as one might like, because it forces the type indices to
--   be the same, so it can only be used to compare two finite or two
--   infinite durations.  To compare durations in general, use
--   'compareDuration'.
instance Ord n => Ord (Duration f n) where
  compare (Duration n1) (Duration n2) = compare n1 n2
  compare Forever Forever             = EQ

-- | Compare two durations.
compareDuration :: Ord n => Duration f1 n -> Duration f2 n -> Ordering
compareDuration Forever       Forever       = EQ
compareDuration (Duration _)  Forever       = LT
compareDuration Forever       (Duration _)  = GT
compareDuration (Duration n1) (Duration n2) = compare n1 n2

-- XXX make an Additive instance instead of Num?

-- | /Finite/ durations inherit the additive structure of the
--   underlying numeric type.  (Note that it does not actually make
--   sense to multiply durations.)  To add durations in general, see
--   'addDuration'.
--
--   This instance also gives us the convenience of 'fromInteger', so
--   numeric literals can be used as finite durations.
instance Num n => Num (Duration F n) where
  fromInteger               = toDuration . fromInteger
  negate = error "use negated to negate a duration"
  (+)    = error "use ^+^ to add durations"
  (*)    = error "multiplying durations makes no sense"
  abs (Duration n)          = Duration (abs n)
  signum = error "signum on durations makes no sense"

instance Additive (Duration F) where
  zero = Duration 0

-- | A wrapper function to convert a numeric value into a finite duration.
toDuration :: n -> Duration F n
toDuration = Duration

-- | An unwrapper function to turn a duration into a numeric value.
fromDuration :: Duration f n -> Maybe n
fromDuration Forever      = Nothing
fromDuration (Duration n) = Just n

-- | Like 'fromDuration' when you know you have a finite duration.
fromFDuration :: Duration F n -> n
fromFDuration (Duration n) = n

-- | Add two durations.  If either one is infinite, so is the result;
--   finite durations add normally.
addDuration :: Num n => Duration f1 n -> Duration f2 n -> Duration (f1 ⊔ f2) n
addDuration Forever      _            = Forever
addDuration (Duration _) Forever      = Forever
addDuration (Duration a) (Duration b) = Duration (a + b)

-- | The maximum of two durations.
maxDuration :: Ord n => Duration f1 n -> Duration f2 n -> Duration (f1 ⊔ f2) n
maxDuration Forever      _            = Forever
maxDuration (Duration _) Forever      = Forever
maxDuration (Duration a) (Duration b) = Duration (max a b)

-- | The minimum of two durations.
minDuration :: (Num n, Ord n) => Duration f1 n -> Duration f2 n -> Duration (f1 ⊓ f2) n
minDuration Forever Forever           = Forever
minDuration Forever (Duration b)      = Duration b
minDuration (Duration a) Forever      = Duration a
minDuration (Duration a) (Duration b) = Duration (min a b)

-- could make ISemigroup, IMonoid classes...

