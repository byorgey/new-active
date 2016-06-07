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

newtype Horiz n a = Horiz { getHoriz :: Active F n a }

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
      n2   = t * n1
      f n  = f1 (n*t)

backwards :: Num n => Active F n a -> Active F n a
backwards (Active (Duration n1) f1) =  Active (Duration n1) f
      where
        f n =  f1 (n1 - n) 
              
runActive :: Ord n => Active f n a -> n -> a
runActive (Active (Duration n1) a1) t
    | t > n1 = error "t1 can't be bigger than n1"
    | otherwise             = a1 (t)
 --  | (0 < t  && t <= n1)   = a1 (t)
 --  | otherwise             = error "Need t == n1"

truncateDuration :: (Ord n, Num n) => Active F n a -> Active F n a -> Active F n a
truncateDuration (Active (Duration t1) a1) (Active (Duration t2) a2)
   | t1 < t2   = Active (Duration t1) a3
   | t2 < t1   = Active (Duration t2) a4
   | otherwise = error "one Active has to be shorter than the other"
   where 
     a3 x = a1 (t2-t1)
     a4 x = a2 (t1-t2)   

matchShorter :: (Ord n, Fractional n) => Active f n a -> Active f n a -> Active f n a
matchShorter (Active (Duration t1) a1) (Active (Duration t2) a2)
    | (t1 < t2) && t1 > 0   = stretch x (Active (Duration t1) a1)
    | (t2 < t1) && t2 > 0   = stretch y (Active (Duration t2) a2)
    where
      x = (t2 / t1)
      y = (t1 / t2)
      
matchDuration :: (Ord n, Fractional n, Num n) => Active f n a -> Active f n a -> Active f n a
matchDuration (Active (Duration t1) a1) (Active (Duration t2) a2) =
  if (t1 == t2)
    then stretch 1 (Active (Duration t1) a1)
    else stretch x (Active (Duration t1) a1)
        where 
          x = (t2*t2) / t1

stretchTo :: (Ord n,  Fractional n) => n -> Active F n a -> Active F n a
stretchTo n (Active (Duration t1) a1) = stretch x (Active (Duration t1) a1)
     where
       x = n/t1

discrete :: Num n => [Int] -> Active f n a
discrete [] = error "Can't produce an Active without values"
discrete (x:xs) = (Active (Duration t1) take 1 cycle (x:xs))
-- use map
   where
     t1 = length [xs]
     

{- instance IApplicative f =>  --f i (a -> b) -> f j a -> f (i :*: b j) b
  type Id = 
  type PureType <**>
  type AppC
  type AppType
  pure :: a -> f Id a 



instance IApplicative f => Applicativey (f :: k -> * -> *) (a :: *) (b :: *) (i :: k) (j :: k) where
  type AppC f = IApplicative
  type PureType f a b = a -> f Id a
  type AppType f a b i j = f i (a -> b) -> f j a -> f (i :*: j) b

  pure = ipure
  (<*>) = (<:*>)

vertical :: (Num n, Ord n, Semigroup a) => Active f n a -> Active f n a -> Active f n a 
vertical (Active (Duration t1) f1) (Active (Duration t2) f2)
    | t1 < t2    = (Active (Duration t1) g)
    | t2 < t1    = (Active (Duration t2) h)
     where 
       g x = f1 t2--(runActive (Active (Duration t2) f2) t2)
       h y = f2 t1 --(runActive (Active (Duration t1) f1) t1) -}
     

------------------------------------------------------------

-- Functions that should be written:

-- vertical, i.e. parallel composition                            

-- stretch    -- stretch by a given factor                         DONE

-- stretchTo  -- stretch a finite Active to a specific duration   DONE
-- specify to a certain length (lets say 5, find the factor that gets me there)

-- matchDuration     -- stretch a finite Active to match the duration of another    DONE
--like stretchTo, i want two actives to have same length, one length matches the other 

-- backwards  -- run a finite Active backwards                     DONE

-- snapshot   -- get a value at a specific time
-- snapShot :: n -> Active f n a -> Active I n a
-- returns an infitine active of constant value a. Not a single a value

-- discrete   -- make an Active from a discrete list of values
-- took a picture, list [1, 2, 3]

-- simulate   -- sample an Active to generate a list of values

-- runActive  -- extract a value at time t from Active             Done