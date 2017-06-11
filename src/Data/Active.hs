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
  , active, instant, lasting, always
  , ui, interval, dur
  , (<#>)

    -- * Running/sampling

  , runActive, start, end, simulate

    -- * Sequential composition
    -- $seq

  , (->-), (->>), (>>-), (-<>-)
  , movieNE, movie, Sequential(..)

    -- * Parallel composition
    -- $par
  , (<+>), stackNE, stack, (<->)

    -- * Other combinators

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
--   @'instant' a = 'lasting' 0 a@
instant :: Num n => a -> Active n F a
instant = lasting 0

-- | A constant value lasting for the specified duration.
--
--   @'lasting' d = 'active' (Duration d) . const = 'cut' d . always@
lasting :: Num n => n -> a -> Active n F a
lasting d = active (Duration d) . const

-- | The unit interval: the identity function on the interval \( [0,1] \).
ui :: Num n => Active n F n
ui = active 1 id

-- | @interval a b@ varies linearly from \( a \) to \( b \) over a
--   duration of \( b - a \).  That is, it represents the function \( d \mapsto a + d \).
interval :: Num n => n -> n -> Active n F n
interval a b = active (toDuration (b - a)) (a+)

-- | @dur@ is the infinite active value representing the function
--   \( d \mapsto d \).  It is called @dur@ since it can be thought of as
--   representing "the current duration" at any point in time.
dur :: Active n I n
dur = active Forever id

(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip (<$>)

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

-- | Generate a list of "frames" or "samples" taken at regular
--   intervals from an 'Active' value.  The first argument is the number
--   of samples per unit time.  That is, @simulate f a@ samples @a@ at
--   times \( 0, \frac 1 f, \frac 2 f, \dots \), ending at the last multiple of
--   \(1/f\) which is not greater than the duration.  The list will be
--   infinite iff the 'Active' is.
simulate :: (Eq n, Fractional n, Enum n) => n -> Active n f a -> [a]
simulate 0 _ = error "Frame rate can't equal zero"
simulate n (Active (Duration d) f) = map f [0, 1/n .. d]
simulate n (Active Forever      f) = map f [0, 1/n ..]

--------------------------------------------------
-- Sequential composition

-- $seq
-- This is a paragraph about sequential composition.

infixr 4 ->-, ->>, >>-, -<>-

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
--   sequential composition are instead defined for the 'Sequential'
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

-- | A newtype wrapper for finite 'Active' values.  The 'Semigroup'
--   and 'Monoid' instances for this wrapper use sequential rather
--   than parallel composition (specifically, ('->-')).
newtype Sequential n a = Sequential { getSequential :: Active n F a }

instance (Num n, Ord n, Semigroup a) => Semigroup (Sequential n a) where
  Sequential a1 <> Sequential a2 = Sequential (a1 ->- a2)

instance (Num n, Ord n, Monoid a, Semigroup a) => Monoid (Sequential n a) where
  mempty  = Sequential (instant mempty)
  mappend = (<>)

-- | Sequence a nonempty list of finite actives together, via ('->-'),
--   but using a balanced fold (which can be more efficient than the
--   usual linear fold).
movieNE :: (Num n, Ord n, Semigroup a) => NonEmpty (Active n F a) -> Active n F a
movieNE = getSequential . foldB1 . coerce

-- | A balanced binary fold.
foldB1 :: Semigroup a => NonEmpty a -> a
foldB1 (a :| as) = maybe a (a <>) (foldBM as)
  where
    foldBM :: Semigroup a => [a] -> Maybe a
    foldBM = getOption . foldB (<>) (Option Nothing) . map (Option . Just)

    foldB :: (a -> a -> a) -> a -> [a] -> a
    foldB _   z []   = z
    foldB _   _ [a]  = a
    foldB (&) z as   = foldB (&) z (pair (&) as)

    pair _   []         = []
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

----------------------------------------
-- Unioning parallel composition

infixr 6 <+>

-- | Unioning parallel composition.  The duration of @x <+> y@ is the
--   /maximum/ of the durations of @x@ and @y@.  Where they are both
--   defined, the values are combined with ('<>').  Where only one is
--   defined, its value is simply copied.
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

-- | If @a@ is a 'Semigroup', then 'Active n f a' forms a 'Semigroup'
--   under unioning parallel composition.  Notice that the two
--   arguments of ('<>') are restricted to be either both finite or
--   both infinite; ('<+>') is strictly more general since it can
--   combine active values with different finitudes.
instance (Semigroup a, Num n, Ord n) => Semigroup (Active n f a) where
  (<>) = (<+>)

-- | If @a@ is a 'Monoid', then 'Active n F a' forms a 'Monoid' under
--   unioning parallel composition.  The identity element is
--   @'instant' 'mempty'@, the same as the identity element for the
--   sequential composition monoid (see 'Sequential').
instance (Monoid a, Semigroup a, Num n, Ord n) => Monoid (Active n F a) where
  mempty = instant mempty
  mappend = (<>)

-- | \"Stack\" a nonempty list of active values via unioning parallel
--   composition.  This is actually just a synonym for 'sconcat'.
stackNE :: (Semigroup a, Num n, Ord n) => NonEmpty (Active n f a) -> Active n f a
stackNE = sconcat

-- | Like 'stackNE', but on a list for convenience.  Calling 'stack'
--   on the empty list is a runtime error.
stack :: (Semigroup a, Num n, Ord n) => [Active n f a] -> Active n f a
stack = sconcat . NE.fromList

----------------------------------------
-- Intersecting parallel composition

instance IFunctor (Active n) where
  imap f (Active d1 g) = Active d1 (f . g)

-- | 'Active n' is an 'IApplicative', somewhat akin to 'ZipList':
--   * 'ipure' creates an infinite constant value.
--   * @f '<:*>' x@ applies @f@ to @x@ pointwise, taking the minimum
--     duration of @f@ and @x@.
instance (Num n, Ord n) => IApplicative (Active n) where
  type Id = I
  type (:*:) i j = i ⊓ j
  ipure = always
  Active d1 f1 <:*> Active d2 f2 = Active (d1 `minDuration` d2) (f1 <*> f2)

-- | @'always' x@ creates an infinite 'Active' which is constantly
--   'x'.  A synonym for 'ipure', but with no type class constraints
--   on @n@.
always :: a -> Active n I a
always a = Active Forever (const a)

infixr 6 <->

-- | Intersecting parallel composition.  The duration of @x '<->' y@ is
--   the /minimum/ of the durations of @x@ and @y@.
--
--   Note that this is a special case of the 'IApplicative' instance
--   for 'Active'; in fact, it is equivalent to @'iliftA2' ('<>')@.
(<->) :: (Semigroup a, Num n, Ord n)
      => Active n f1 a -> Active n f2 a -> Active n (f1 ⊓ f2) a
(<->) = iliftA2 (<>)

--------------------------------------------------
-- Other combinators

-- | Stretch an active value by a positive factor.  For example,
--   @'stretch' 2 a@ is twice as long as @a@ (and hence half as fast).
--   Conversely, @'stretch' (1/2) a@ is half as long as @a@, and hence
--   twice as fast.
--
--   This actually works perfectly well on infinite active values,
--   despite the fact that it no longer makes sense to say that the
--   result is "x times as long" as the input; it simply stretches out
--   the values along the number line.
stretch :: (Fractional n, Ord n) => n -> Active n f a -> Active n f a
stretch s a@(Active d f)
  | s <= 0 = error "Nonpositive stretch factor"
  | otherwise = Active (s *^ d) (\t -> f (t/s))

-- | Like 'stretch', but allows negative stretch factors, which
--   reverse the active.  As a result, it is restricted to only finite
--   actives.
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

