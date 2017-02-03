{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

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
import qualified Data.Vector          as V

------------------------------------------------------------
-- Time
------------------------------------------------------------

data Duration f n where
  Duration :: n -> Duration F n
  Forever  ::      Duration I n

deriving instance Show n => Show (Duration f n)
deriving instance Functor (Duration f)

instance Eq n => Eq (Duration f n) where
  Duration n1 == Duration n2 = n1 == n2
  Forever     == Forever     = True

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

minDuration :: (Num n, Ord n) => Duration f1 n -> Duration f2 n -> Duration (Isect f1 f2) n
minDuration Forever Forever           = Forever
minDuration Forever (Duration b)      = Duration b
minDuration (Duration a) Forever      = Duration a
minDuration (Duration a) (Duration b) = Duration (min a b)

-- could make ISemigroup, IMonoid classes...

------------------------------------------------------------
--  Active
------------------------------------------------------------

data Active :: * -> Finitude -> * -> * where
  Active   :: Duration f n -> (n -> a) -> Active n f a

ui :: Num n => Active n F n
ui = Active 1 id

interval :: Num n => n -> n -> Active n F n
interval a b = Active (toDuration (b - a)) (a+)

dur :: Active n I n
dur = Active Forever id

infixr 4 ->>
(->>) :: (Semigroup a, Num n, Ord n) => Active n F a -> Active n f a -> Active n f a
(Active d1@(Duration n1) f1) ->> (Active d2 f2) = Active (addDuration d1 d2) f
  where
    f n | n <  n1   = f1 n
        | n == n1   = f1 n <> f2 0
        | otherwise = f2 (n - n1)

infix 4 ->-
(->-) :: (Semigroup a, Num n, Ord n) => Active n F a -> Active n f a -> Active n f a
(Active d1@(Duration n1) f1) ->- (Active d2 f2) = Active (addDuration d1 d2) f
  where
    f n | n < n1  = f1 n
        | n >= n1 = f1 n1 <> f2 (n - n1)

newtype Horiz n a = Horiz { getHoriz :: Active n F a }

instance (Num n, Ord n, Semigroup a) => Semigroup (Horiz n a) where
  Horiz a1 <> Horiz a2 = Horiz (a1 ->> a2)

sequenceNE :: (Num n, Ord n, Semigroup a) => NonEmpty (Active n F a) -> Active n F a
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
movie :: (Num n, Ord n, Semigroup a) => [Active n F a] -> Active n F a
movie []     = error "Can't make empty movie!"
movie (a:as) = sequenceNE (a :| as)

startVal :: Num n => Active n f a -> a
startVal (Active _ f) = f 0

endVal :: Active n F a -> a
endVal (Active (Duration d) f) = f d

stretch :: (Fractional n, Ord n) => n -> Active n F a -> Active n F a
stretch s a@(Active (Duration d) f)
    | s > 0     = Active (Duration (d*s)) (f . (/s))
    | s < 0     = stretch (abs s) (backwards a)
    | otherwise = error "Active stretched by zero"

backwards :: Num n => Active n F a -> Active n F a
backwards (Active (Duration d) f) =  Active (Duration d) (f . (d-))

runActive :: Ord n => Active n f a -> n -> a
runActive (Active (Duration d) f) t
  | t > d     = error "Active evaluated past its duration"
  | otherwise = f t

matchDuration :: (Ord n, Fractional n) => Active n f a -> Active n f a -> Active n f a
matchDuration a@(Active (Duration d1) _) (Active (Duration d2) _) = stretch (d2/d1) a

stretchTo :: (Ord n,  Fractional n) => n -> Active n F a -> Active n F a
stretchTo n (Active (Duration d) f) = stretch (n/d) (Active (Duration d) f)

discrete :: (RealFrac n, Ord n) => [a] -> Active n F a
discrete [] = error "Data.Active.discrete must be called with a non-empty list."
discrete xs = (Active 1 f)
  where
    f t
      | t == 1    = V.unsafeLast v
      | otherwise = V.unsafeIndex v $ floor (t * fromIntegral (V.length v))
    v = V.fromList xs

snapshot :: (Fractional n) => n -> Active n f a -> Active n I a
snapshot t (Active _ f) = Active Forever (const (f t))

simulate :: (Eq n, Fractional n, Enum n) => n -> Active n f a -> [a]
simulate 0 _ = error "Frame rate can't equal zero"
simulate n (Active (Duration d) f) = map f [0, 1/n .. d]
simulate n (Active Forever      f) = map f [0, 1/n ..]

instance (Num n, Ord n) => IApplicative (Active n) where
  type Id = I
  type (:*:) i j = Isect i j
  ipure a = Active Forever (const a)
  Active d1 f1 <:*> Active d2 f2 = Active (d1 `minDuration` d2) (f1 <*> f2)

instance IFunctor (Active n) where
  imap f (Active d1 g) = Active d1 (f . g)

instance (Semigroup a, Num n, Ord n) => Semigroup (Active n f a) where
  a1 <> a2 = (<>) <:$> a1 <:*> a2

stack :: (Semigroup a, Num n, Ord n) => [Active n f a] -> Active n f a
stack = sconcat . NE.fromList

(<:>) :: (Semigroup a, Num n, Ord n)
      => Active n f1 a -> Active n f2 a -> Active n (f1 :*: f2) a
a1 <:> a2 = (<>) <:$> a1 <:*> a2

cut :: (Num n, Ord n) => n -> Active n f a -> Active n F a
cut c (Active d f) = Active ((Duration c) `minDuration` d) f
