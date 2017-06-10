{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
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

module Data.Active
  ( -- * Durations
    -- | Re-exported for convenience.

    module Data.Duration

    -- * The Active type
  , Active

    -- * Constructing
  , active, instant
  , ui, interval, dur

    -- * Running/sampling

  , runActive, start, end, simulate

    -- * Combinators

    -- ** Sequential composition
    -- $seq

  , (->-), (->>), (>>-), (-<>-)
  , movieNE, movie, Horiz(..)

    -- ** Parallel composition
    -- $par
  , (<+>), (<->), stack

    -- ** Other combinators

  , stretch, stretchTo, matchDuration
  , cut, backwards, snapshot
  ) where

import           Data.Coerce

import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import           Data.Semigroup
import           Linear.Vector

import           Data.Duration
import           Control.IApplicative
import           Data.Finitude
import qualified Data.Vector          as V

-- XXX go through and include diagrams of everything!

------------------------------------------------------------
--  Active
------------------------------------------------------------

-- | A value of type @Active n f a@ is a time-varying value of type
--   @a@ with a given duration.
--
--   * @f@ is an index indicating whether the duration is finite or
--   infinite.
--   * @n@ is the numeric type used for durations.  Any numeric type
--     can be used, although it makes the most sense for types with
--     decidable equality, such as 'Rational'.
--   * @a@ is the type of the values.
--
--   If the duration is infinite, it can be thought of as a function
--   \( [0,+\infty) \to a \); if it has a finite duration \( d \), it
--   can be thought of as a function \( [0,d] \to a \) (note in
--   particular that the interval is /closed/ on both ends: the
--   function is defined at \(0\) as well as at the duration \(d\)).
--
--   @Active n f@ is a @Functor@, and @Active n@ is an 'IFunctor' and
--   'IApplicative' (the 'IApplicative' instance in particular is how
--   one does parallel composition of actives).  XXX mention Semigroup
--
--   This definition is intentionally abstract, since the
--   implementation may change in the future to enable additional
--   optimizations.  To construct an 'Active' value, see the 'active'
--   function as well as 'ui', 'interval', 'dur', 'discrete',
--   and the various combinators.
data Active :: * -> Finitude -> * -> * where
  Active   :: Duration f n -> (n -> a) -> Active n f a
  deriving Functor

--------------------------------------------------
-- Constructing

-- | Constructor for 'Active' values, given a duration \(d\) and a
--   function from \([0,d] \to a\).  The given function need not be
--   defined outside the interval, although in Haskell there is no way
--   to indicate this in the types.  For example,
--
--   > active 3 (\d -> if d <= 3 then d*2 else error "o noes!")
--
--   is a well-defined, total 'Active' value.
active :: Duration f n -> (n -> a) -> Active n f a
active = Active

-- | A value of duration zero.
--
--   @'instant' a = 'active' 0 (const a)@.
instant :: Num n => a -> Active n F a
instant a = active 0 (const a)

-- | The unit interval: the identity function on the interval \( [0,1] \).
ui :: Num n => Active n F n
ui = Active 1 id

-- | @interval a b@ varies linearly from \( a \) to \( b \) over a
--   duration of \( b - a \).  That is, it represents the function \( d \mapsto a + d \).
interval :: Num n => n -> n -> Active n F n
interval a b = Active (toDuration (b - a)) (a+)

-- | @dur@ is the infinite active value representing the function
--   \( d \mapsto d \).  It is called @dur@ since it can be thought of as
--   representing "the current duration" at any point in time.
dur :: Active n I n
dur = Active Forever id

--------------------------------------------------
-- Running/sampling

-- | The semantic function for 'Active': interpret an 'Active' value
--   as a function from durations.  Looked at another way, this is how
--   you can sample an 'Active' value at a given duration.  Note that
--   attempting to evaluate a finite active past its duration results
--   in a runtime error. (Unfortunately, in Haskell it would be very
--   difficult to rule this out statically.)
runActive :: Ord n => Active n f a -> n -> a
runActive (Active d f) t
  = case compareDuration (Duration t) d of
      GT -> error "Active value evaluated past its duration."
      _  -> f t

-- | Like 'runActive', but return a total function that returns
--   @Nothing@ when queried outside its range.
runActiveMaybe :: Ord n => Active n f a -> n -> Maybe a
runActiveMaybe (Active d f) t
  = case compareDuration (Duration t) d of
      GT -> Nothing
      _  -> Just (f t)

-- | Extract the value at the beginning of an 'Active'.
start :: Num n => Active n f a -> a
start (Active _ f) = f 0

-- | Extract the value at the end of a finite 'Active'.
end :: Active n F a -> a
end (Active (Duration d) f) = f d

-- | XXX document me
simulate :: (Eq n, Fractional n, Enum n) => n -> Active n f a -> [a]
simulate 0 _ = error "Frame rate can't equal zero"
simulate n (Active (Duration d) f) = map f [0, 1/n .. d]
simulate n (Active Forever      f) = map f [0, 1/n ..]

--------------------------------------------------
-- Sequential composition

-- $seq
-- This is a paragraph about sequential composition.

infixr 4 ->>

-- | Sequential composition.
--
--   @x ->- y@ is the active which behaves first as @x@, and then as
--   @y@; the total duration is the sum of the durations of @x@ and
--   @y@.  The value of @x ->- y@ at the instant @x@ and @y@ overlap
--   is the composition of @x@ and @y@'s values under ('<>').
--
--   Note that @x@ must be finite, but @y@ may be infinite.
--
--   See also ('-<>-'), where the final value of @x@ "accumulates".
--   In the case that the values of @x@ and @y@ do not have a
--   'Semigroup' instance, one can, for example, wrap them in 'Last',
--   so that the value from @x@ will be ignored and the value from @y@
--   taken. In fact, the ('->>') and ('>>-') operators are provided
--   for convenience which handle this common situation.
--
--   Finite active values form a semigroup under horizontal
--   composition as long as the value type @a@ is a 'Semigroup';
--   additionally, if @a@ is a 'Monoid', then finite active values are
--   as well, with @'instant' 'mempty'@ as the identity.  However, the
--   'Semigroup' and 'Monoid' instances for 'Active' are for parallel
--   rather than sequential composition.  The instances with
--   sequential composition are instead defined for the 'Horiz'
--   newtype wrapper.
(->-) :: (Semigroup a, Num n, Ord n) => Active n F a -> Active n f a -> Active n f a
(Active d1@(Duration n1) f1) ->- (Active d2 f2) = Active (addDuration d1 d2) f
  where
    f n | n <  n1   = f1 n
        | n == n1   = f1 n <> f2 0
        | otherwise = f2 (n - n1)

-- | Sequential composition, preferring the value from the right-hand
--   argument at the instant of overlap; see ('->-').
--
--   XXX example / (picture)
(->>) :: forall n f a. (Num n, Ord n) => Active n F a -> Active n f a -> Active n f a
a1 ->> a2 = coerce ((coerce a1 ->- coerce a2) :: Active n f (Last a))

-- | Sequential composition, preferring the value from the left-hand
--   argument at the instant of overlap; see ('->-').
(>>-) :: forall n f a. (Num n, Ord n) => Active n F a -> Active n f a -> Active n f a
a1 >>- a2 = coerce ((coerce a1 ->- coerce a2) :: Active n f (First a))

infix 4 -<>-
-- | Accumulating sequential composition.
--
--   @x -<>- y@ first behaves as @x@, and then behaves as @y@, except
--   that the final value of @x@ is combined via ('<>') with every
--   value from @y@.  For example:
--
--   XXX look into actual executable doc examples
--
--   > > let x = active 3 Sum :: Active Rational F (Sum Rational)
--   > > let a1 = x ->- x
--   > > let a2 = x -<>- x
--   > > map (numerator . getSum . runActive a1) [0 .. 6]
--   >   [0,1,2,3,1,2,3]
--   > > map (numerator . getSum . runActive a2) [0 .. 6]
--   >   [0,1,2,3,4,5,6]
--
--   @(-<>-)@ satisfies the law:
--
--   @x -<>- y = x ->> (('end' x '<>') '<$>' y)@
(-<>-) :: (Semigroup a, Num n, Ord n) => Active n F a -> Active n f a -> Active n f a
(Active d1@(Duration n1) f1) -<>- (Active d2 f2) = Active (addDuration d1 d2) f
  where
    f n | n < n1  = f1 n
        | n >= n1 = f1 n1 <> f2 (n - n1)

-- XXX change the name of Horiz

-- | A newtype wrapper for finite 'Active' values.  The 'Semigroup'
--   and 'Monoid' instances for this wrapper use sequential rather
--   than parallel composition.
newtype Horiz n a = Horiz { getHoriz :: Active n F a }

instance (Num n, Ord n, Semigroup a) => Semigroup (Horiz n a) where
  Horiz a1 <> Horiz a2 = Horiz (a1 ->- a2)

instance (Num n, Ord n, Monoid a, Semigroup a) => Monoid (Horiz n a) where
  mempty = Horiz (instant mempty)
  mappend = (<>)

-- | Sequence a nonempty list of finite actives together, via ('->-'),
--   but using a balanced fold (which can be more efficient than the
--   usual linear fold).
movieNE :: (Num n, Ord n, Semigroup a) => NonEmpty (Active n F a) -> Active n F a
movieNE = getHoriz . foldB1 . NE.map Horiz

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

-- | A variant of 'sequenceNE' defined on lists instead of 'NonEmpty'
--   for convenience; @movie []@ is a runtime error.
movie :: (Num n, Ord n, Semigroup a) => [Active n F a] -> Active n F a
movie []     = error "Can't make empty movie!"
movie (a:as) = movieNE (a :| as)

--------------------------------------------------
-- Parallel composition

-- $par
-- This is a paragraph about parallel composition.

instance (Num n, Ord n) => IApplicative (Active n) where
  type Id = I
  type (:*:) i j = i ⊓ j
  ipure a = Active Forever (const a)
  Active d1 f1 <:*> Active d2 f2 = Active (d1 `minDuration` d2) (f1 <*> f2)

instance IFunctor (Active n) where
  imap f (Active d1 g) = Active d1 (f . g)

instance (Semigroup a, Num n, Ord n) => Semigroup (Active n f a) where
  (<>) = (<+>)

stack :: (Semigroup a, Num n, Ord n) => [Active n f a] -> Active n f a
stack = sconcat . NE.fromList

stackNE :: (Semigroup a, Num n, Ord n) => NonEmpty (Active n f a) -> Active n f a
stackNE = sconcat

(<+>) :: (Semigroup a, Num n, Ord n)
      => Active n f1 a -> Active n f2 a -> Active n (f1 ⊔ f2) a
a1@(Active d1 _) <+> a2@(Active d2 _)
  = Active (d1 `maxDuration` d2)
           (\t -> case (runActiveMaybe a1 t, runActiveMaybe a2 t) of
                    (Just b1, Just b2) -> b1 <> b2
                    (Just b, _)        -> b
                    (_, Just b)        -> b
           )
                    -- (Nothing, Nothing) case can't happen.

(<->) :: (Semigroup a, Num n, Ord n)
      => Active n f1 a -> Active n f2 a -> Active n (f1 ⊓ f2) a
(<->) = iliftA2 (<>)

--------------------------------------------------
-- Other combinators

stretch :: (Fractional n, Ord n) => n -> Active n f a -> Active n f a
stretch s a@(Active d f)
  | s <= 0 = error "Nonpositive stretch factor"
  | otherwise = Active (s *^ d) (\t -> f (t/s))

-- Allows negative stretching?
stretch' :: (Fractional n, Ord n) => n -> Active n F a -> Active n F a
stretch' s a@(Active (Duration d) f)
    | s > 0     = Active (Duration (d*s)) (f . (/s))
    | s < 0     = stretch (abs s) (backwards a)
    | otherwise = error "stretch' 0"

backwards :: Num n => Active n F a -> Active n F a
backwards (Active (Duration d) f) =  Active (Duration d) (f . (d-))

matchDuration :: (Ord n, Fractional n) => Active n F a -> Active n F a -> Active n F a
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

cut :: (Num n, Ord n) => n -> Active n f a -> Active n F a
cut c (Active d f) = Active ((Duration c) `minDuration` d) f

--------------------------------------------------

{- Reasons we need/want indexing of Finitude:

   If we want to support infinite Actives, then these functions need
   to know about finitude:

   - end
   - backwards
   - stretchTo
   - matchDuration

   
-}

