{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Active
-- Copyright   :
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :
--
--
-----------------------------------------------------------------------------

module Data.Active where

import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import           Data.Semigroup

import           Control.IApplicative
import           Data.Finitude

------------------------------------------------------------
-- Time
------------------------------------------------------------

data Duration f n where
  Duration :: n -> Duration F n
  Forever  ::      Duration I n
  --, Show, Read, Enum, Num, Fractional, Real, RealFrac, Functor)

instance Eq n => Eq (Duration f n) where
  Duration n1 == Duration n2 = n1 == n2
  Forever     == Forever     = True
  _           == _           = False

instance Num n => Num (Duration F n) where
  fromInteger               = toDuration . fromInteger
  negate (Duration n)       = Duration (negate n)
  Duration n1 + Duration n2 = Duration (n1 + n2)
  Duration n1 * Duration n2 = Duration (n1 * n2)
  abs (Duration n)          = Duration (abs n)
  signum (Duration n)       = Duration (signum n)

-- | A convenient wrapper function to convert a numeric value into a duration.
toDuration :: n -> Duration F n
toDuration = Duration

-- | A convenient unwrapper function to turn a duration into a numeric value.
fromDuration :: Duration f n -> Maybe n
fromDuration Forever      = Nothing
fromDuration (Duration n) = Just n

fromFDuration :: Duration F n -> n
fromFDuration (Duration n) = n

addDuration :: Num n => Duration f1 n -> Duration f2 n -> Duration (Union f1 f2) n
addDuration Forever      _            = Forever
addDuration (Duration _) Forever      = Forever
addDuration (Duration a) (Duration b) = Duration (a + b)

-- could make ISemigroup, IMonoid classes...

------------------------------------------------------------
--  Active
------------------------------------------------------------

data Active :: Finitude -> * -> * -> * where
  Active   :: Duration f n -> (n -> a) -> Active f n a

ui :: Num n => Active F n n
ui = Active 1 id

interval :: Num n => n -> n -> Active F n n
interval a b = Active (toDuration (b - a)) (a+)

dur :: Active I n n
dur = Active Forever id

(->>) :: (Semigroup a, Num n, Ord n) => Active F n a -> Active f n a -> Active f n a
(Active d1@(Duration n1) f1) ->> (Active d2 f2) = Active (addDuration d1 d2) f
  where
    f n | n <  n1   = f1 n
        | n == n1   = f1 n <> f2 0
        | otherwise = f2 (n - n1)

newtype Horiz n a = Horiz { getHoriz :: Active F n a } -- comment

instance (Num n, Ord n, Semigroup a) => Semigroup (Horiz n a) where
  Horiz a1 <> Horiz a2 = Horiz (a1 ->> a2)

sequenceNE :: (Num n, Ord n, Semigroup a) => NonEmpty (Active F n a) -> Active F n a
sequenceNE = getHoriz . foldB1 . NE.map Horiz

foldB1 :: Semigroup a => NonEmpty a -> a
foldB1 (a :| as) = maybe a (a <>) (foldBM as)
  where
    foldBM :: Semigroup a => [a] -> Maybe a
    foldBM = getOption . foldB (<>) (Option Nothing) . map (Option . Just)

    foldB :: (a -> a -> a) -> a -> [a] -> a
    foldB _   z []   = z
    foldB _   _ [a]  = a
    foldB (&) z as   = foldB (&) z (pair (&) as)

    pair _   [a]        = [a]
    pair (&) (a1:a2:as) = (a1 & a2) : pair (&) as

-- For convenience
movie :: (Num n, Ord n, Semigroup a) => [Active F n a] -> Active F n a
movie []     = error "Can't make empty movie!"
movie (a:as) = sequenceNE (a :| as)


startVal :: Num n => Active f n a -> a
startVal (Active _ f) = f 0

endVal :: Active F n a -> a
endVal (Active (Duration n) f) = f n


stretch :: (Num n, Ord n) => n -> Active F n a -> Active F n a
stretch t (Active (Duration n1) f1)
    | t > 0     = Active (Duration n2) f
    | otherwise = error "Can only stretch by rational numbers > 0"
    where
      n2 = t * n1
      f n  = f1 (n*t)


{-backwards :: Active F n a -> Active F n a
backwards (Active (Duration n1) f1) =  Active (Duration n2) f1
      where
        n2 = reverse n1
 -}       
------------------------------------------------------------

-- Functions that should be written:

-- vertical, i.e. parallel composition

-- stretch    -- stretch by a given factor
-- stretchTo  -- stretch a finite Active to a specific duration
-- during     -- stretch a finite Active to match the duration of another
-- backwards  -- run a finite Active backwards

-- snapshot   -- get a value at a specific time

-- discrete   -- make an Active from a discrete list of values
-- simulate   -- sample an Active to generate a list of values
