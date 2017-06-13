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
-- Module      :  Active
-- Copyright   :  2011-2017 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- active is a small EDSL for building continuous, time-varying values
-- of arbitrary type. It is particularly useful for building media
-- such as animations, audio clips, and the like, but it is often
-- useful to have other values that vary over time (vectors, colors,
-- filters, volume levels...) and be able to create and use them in
-- the service of constructing time-varying media.
--
-- XXX basic ideas.  Duration, time-invariant.  (After all, nothing
-- constructed will really be fixed in time...)  Sequential and
-- parallel composition.
--
-- Rational vs. floating-point durations.
--
-----------------------------------------------------------------------------

module Active
  ( -- * Durations

    -- | The module defining durations and operations on them is
    --   re-exported for convenience.

    module Active.Duration

    -- * The Active type
  , Active

    -- * Primitives
  , activeF, activeI, active
  , instant, lasting, always
  , ui, interval, dur
  , (<#>)
  , discreteNE, discrete

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
import           Unsafe.Coerce

import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import           Data.Maybe (fromJust)
import           Data.Semigroup
import qualified Data.Vector          as V
import           Linear.Vector

import           Control.IApplicative
import           Active.Duration


-- XXX go through and include diagrams of everything!

------------------------------------------------------------
--  Active
------------------------------------------------------------

-- | A value of type @Active n f a@ is a time-varying value of type
--   @a@ with a given duration.
--
--   * @f@ is an index indicating whether the duration is finite or
--   infinite.
--   * @n@ is the numeric type used for durations.
--   * @a@ is the type of the values.
--
--   If the duration is infinite, it can be thought of as a function
--   \( [0,+\infty) \to a \); if it has a finite duration \( d \), it
--   can be thought of as a function \( [0,d] \to a \) (note in
--   particular that the interval is /closed/ on both ends: the
--   function is defined at \(0\) as well as at the duration \(d\)).
--
--   @Active n f@ is a @Functor@, and @Active n@ is an 'IApplicative';
--   if @a@ is a 'Semigroup' then @Active n f a@ is as well.  These
--   instances are described in much more detail in the sections on
--   sequential and parallel composition below.
--
--   This definition is intentionally abstract, since the
--   implementation may change in the future to enable additional
--   optimizations.
--
--   Semantically, an 'Active' only needs to be defined on the
--   interval \([0,d]\), although in Haskell there is no way to
--   enforce this with types.  For example,
--
--   > activeF 3 (\d -> if d <= 3 then d*2 else error "o noes!")
--
--   is considered a well-defined, total 'Active' value, even though
--   the provided Haskell function is partial.  Because 'Active' is
--   abstract, it is impossible to ever observe the value of an
--   'Active' past its duration.
--
--   > > let a = activeF 3 (\d -> if d <= 5 then d*2 else error "o noes!")
--   > > runActive a 4
--   > *** Exception: Active value evaluated past its duration.
--
--   Even though in this example the provided Haskell function is
--   defined at the value 4 (in particular it is equal to 8), it is
--   impossible to observe this since the 'Active' has a duration of
--   only 3.

data Active :: * -> Finitude -> * -> * where
  Prim    :: Duration f n -> (n -> a) -> Active n f a
  Fmap    :: Duration f n
          -> (a -> b) -> Active n f a -> Active n f b
  Seq     :: Semigroup a => Duration f n
          -> Active n F a -> Active n f a -> Active n f a
  SeqR    :: Duration f n
          -> Active n F a -> Active n f a -> Active n f a
  SeqL    :: Duration f n
          -> Active n F a -> Active n f a -> Active n f a
  Pure    :: a -> Active n I a
  Ap      :: Duration (f1 ⊓ f2) n
          -> Active n f1 (a -> b) -> Active n f2 a -> Active n (f1 ⊓ f2) b
  Par     :: Semigroup a => Duration (f1 ⊔ f2) n
          -> Active n f1 a -> Active n f2 a -> Active n (f1 ⊔ f2) a
  Rev     :: Active n F a -> Active n F a
  Str     :: Duration f n -> n -> Active n f a -> Active n f a
  Cut     :: Duration F n -> Active n f a -> Active n F a

-- Unexported.  We could get around the need for this with some
-- type-level proofs using singleton machinery and so on, but meh.
assertFinite :: Active n f a -> Active n F a
assertFinite = unsafeCoerce

getDuration :: Active n f a -> Duration f n
getDuration (Prim d _)   = d
getDuration (Fmap d _ _) = d
getDuration (Seq d _ _)  = d
getDuration (SeqR d _ _) = d
getDuration (SeqL d _ _) = d
getDuration (Pure _)     = Forever
getDuration (Ap d _ _)   = d
getDuration (Par d _ _)  = d
getDuration (Rev a)      = getDuration a
getDuration (Str d _ _)  = d
getDuration (Cut d _)    = d

instance Functor (Active n f) where
  fmap f (Fmap d g a) = Fmap d (f . g) a
  fmap f a            = Fmap (getDuration a) f a

-- instance Applicative (Active n I) where

instance IFunctor (Active n) where
  imap = fmap

-- | @'Active' n@ is an 'IApplicative', somewhat akin to 'ZipList':
--
--   * 'ipure' creates an infinite constant value.
--   * @f '<:*>' x@ applies @f@ to @x@ pointwise, taking the minimum
--     duration of @f@ and @x@.
instance (Num n, Ord n) => IApplicative (Active n) where
  type Id = I
  type (:*:) i j = i ⊓ j

  ipure = Pure
  f <:*> x = Ap (minDuration (getDuration f) (getDuration x)) f x

--------------------------------------------------
-- Constructing

-- | Smart constructor for finite 'Active' values, given a finite
--   numeric duration \(d\) and a function from \([0,d] \to a\).
--
--   For example:
--
--   > > let act = activeF (2*pi) (\d -> sin d + cos d) :: Active Double F Double
--   > > mapM_ putStrLn $ simulate 4 (act <#> (\x -> replicate (8 + round (4*x)) '*'))
--   > ************
--   > *************
--   > *************
--   > **************
--   > **************
--   > *************
--   > ************
--   > ***********
--   > **********
--   > *********
--   > *******
--   > ******
--   > *****
--   > ****
--   > ***
--   > **
--   > **
--   > ***
--   > ***
--   > ****
--   > *****
--   > *******
--   > ********
--   > *********
--   > ***********
--   > ************
--
--   Satisfies the law:
--
--   @'activeF' d f = 'interval' d '<#>' f@

activeF :: n -> (n -> a) -> Active n F a
activeF n = Prim (Duration n)

-- | Smart constructor for infinite 'Active' values, given a total
--   function of type \(n \to a\) giving a value of type \(a\) at every
--   time.
--
--   <<#diagram=testDia&width=200>>
activeI :: (n -> a) -> Active n I a
activeI = Prim Forever

-- | Generic smart constructor for 'Active' values, given a 'Duration'
--   and a function on the appropriate interval.
active :: Duration f n -> (n -> a) -> Active n f a
active = Prim

-- | A value of duration zero.
--
--   @'instant' a = 'lasting' 0 a@
instant :: Num n => a -> Active n F a
instant = lasting 0

-- | A constant value lasting for the specified duration.
--
--   This works particularly nicely with postfix function application, a
--   la @(#)@ from the diagrams library.  For example:
--
--   > c :: Active Rational F Char
--   > c = movie
--   >   [ 'a' # lasting 2
--   >   , 'b' # lasting 3
--   >   , 'c' # lasting 1
--   >   ]
--
-- @'lasting' d = 'activeF' d . const
--          = 'cut' d . always@
lasting :: Num n => n -> a -> Active n F a
lasting d = activeF d . const

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

infixl 4 <#>

-- | Backwards 'fmap', that is, a synonym for @'flip' ('<$>')@.  This
--   can be useful when starting from some 'Active' like 'ui',
--   'interval', or 'dur', and then applying a function to it. For
--   example:
--
--   > interval 3 5 <#> \t -> circle 1 # translateX t
(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip (<$>)


--- XXX TODO: redo discreteNE as a sequential composition rather than
--- as a single Prim

-- | Create a "discrete" 'Active' from a nonempty list of values.  The
--   resulting 'Active' has duration 1, and takes on eavh value from
--   the list in turn for a duration of \(1/n\), where \(n\) is the
--   number of items in the list.
--
--   XXX picture --- show step function
--
--   If you want the result to last longer than 1 unit, you can use
--   'stretch'.
discreteNE :: (RealFrac n, Ord n) => NonEmpty a -> Active n F a
discreteNE (a :| as) = activeF 1 f
  where
    f t
      | t == 1    = V.unsafeLast v
      | otherwise = V.unsafeIndex v $ floor (t * fromIntegral (V.length v))
    v = V.fromList (a:as)

-- | Like 'discreteNE', but with a list for convenience.  Calling
--   'discrete' on the empty list is a runtime error.
discrete :: (RealFrac n, Ord n) => [a] -> Active n F a
discrete [] = error "Active.discrete must be called with a non-empty list."
discrete (a : as) = discreteNE (a :| as)

--------------------------------------------------
-- Running/sampling

-- | The semantic function for 'Active': interpret an 'Active' value
--   as a function from durations.  Looked at another way, this is how
--   you can sample an 'Active' value at a given duration.  Note that
--   attempting to evaluate a finite active past its duration results
--   in a runtime error. (Unfortunately, in Haskell it would be very
--   difficult to rule this out statically.)
runActive :: forall n f a. (Fractional n, Ord n) => Active n f a -> n -> a
runActive a t
  = case compareDuration (Duration t) (getDuration a) of
      GT -> error "Active.runActive: Active value evaluated past its duration."
      _  -> go a t
  where
    go :: Active n f' a' -> n -> a'
    go (Prim _ f)    t = f t
    go (Fmap _ f a)  t = f (go a t)

    go (Seq _ a1 a2) t
      | t <  d1   = go a1 t
      | t == d1   = go a1 t <> go a2 (t - d1)
      | otherwise = go a2 (t - d1)
      where
        Duration d1 = getDuration a1
    go (SeqR _ a1 a2) t
      | t < d1    = go a1 t
      | otherwise = go a2 t
      where
        Duration d1 = getDuration a1
    go (SeqL _ a1 a2) t
      | t <= d1   = go a1 t
      | otherwise = go a2 t
      where
        Duration d1 = getDuration a1

    go (Pure a) _   = a
    go (Ap _ af ax)  t = go af t (go ax t)
    go (Par _ a1 a2) t =
      let t' = Duration t in
      case (compareDuration t' (getDuration a1), compareDuration t' (getDuration a2)) of
        (LT, LT) -> go a1 t <> go a2 t
        (LT, _ ) -> go a1 t
        (_ , LT) -> go a2 t
        _        -> error "Active.runActive: internal error, duration past both in Par"
    go (Rev a) t =
      let Duration d = getDuration a in
      go a (d - t)
    go (Str _ k a) t = go a (t/k)
    go (Cut _ a) t   = go a t

-- | Like 'runActive', but return a total function that returns
--   @Nothing@ when queried outside its range.
runActiveMaybe :: (Fractional n, Ord n) => Active n f a -> n -> Maybe a
runActiveMaybe a t
  = case compareDuration (Duration t) (getDuration a) of
      GT -> Nothing
      _  -> Just $ runActive a t

-- | Like 'runActiveMaybe', but return an 'Option' instead of 'Maybe'.
--   Sometimes this is more convenient since the 'Monoid' instance XXX
runActiveOption :: (Fractional n, Ord n) => Active n f a -> n -> Option a
runActiveOption a = Option . runActiveMaybe a

-- | Extract the value at the beginning of an 'Active'.
start :: (Fractional n, Ord n) => Active n f a -> a
start (Prim _ f)    = f 0
start (Fmap _ f a)  = f (start a)
start (Seq _ a1 a2) =
  case getDuration a1 of
    Duration 0 -> start a1 <> start a2
    _          -> start a1
start (SeqR _ a1 a2) =
  case getDuration a1 of
    Duration 0 -> start a2
    _          -> start a1
start (SeqL _ a1 _) = start a1
start (Pure a)      = a
start (Ap _ a1 a2)  = start a1 (start a2)
start (Par _ a1 a2) = start a1 <> start a2
start (Rev a)       = end a
start (Str _ _ a)   = start a
start (Cut _ a)     = start a

-- | Extract the value at the end of a finite 'Active'.
end :: (Fractional n, Ord n) => Active n F a -> a
end (Prim (Duration d) f) = f d
end (Fmap _ f a)          = f (end a)
end (Seq _ a1 a2) =
  case getDuration a2 of
    Duration 0 -> end a1 <> end a2
    _          -> end a2
end (SeqR _ _ a2) = end a2
end (SeqL _ a1 a2) =
  case getDuration a2 of
    Duration 0 -> end a1
    _          -> end a2
end (Ap _ af ax) =
  case compareDuration (getDuration af) (getDuration ax) of

    -- Since the argument to end must be finite, the argument to Ap
    -- with the shorter duration must be finite (or both if they have
    -- the same duration).
    LT -> let Duration afd = getDuration af in (end (assertFinite af)) (runActive ax afd)
    GT -> let Duration axd = getDuration ax in (runActive af axd) (end (assertFinite ax))
    EQ -> end (assertFinite af) (end (assertFinite ax))

end (Par _ a1 a2) =
  case compareDuration (getDuration a1) (getDuration a2) of
    LT -> end a2F
    GT -> end a1F
    EQ -> end a1F <> end a2F
  where
    -- We know end takes a finite Active, and the parallel composition
    -- of two things can only be finite if both are finite.
    a1F = assertFinite a1
    a2F = assertFinite a2

end (Rev a)     = start a
end (Str _ _ a) = end a
end (Cut (Duration d) a) = runActive a d

-- | Generate a list of "frames" or "samples" taken at regular
--   intervals from an 'Active' value.  The first argument is the number
--   of samples per unit time.  That is, @simulate f a@ samples @a@ at
--   times \( 0, \frac 1 f, \frac 2 f, \dots \), ending at the last multiple of
--   \(1/f\) which is not greater than the duration.  The list will be
--   infinite iff the 'Active' is.
simulate :: forall n f a. (Ord n, RealFrac n, Enum n) => n -> Active n f a -> [a]
simulate 0 _ = error "Active.simulate: Frame rate can't equal zero"
simulate frames a = go False 0 (1/frames) a
  where

    -- argh, have to keep track of beginning and ending offset!

    -- Keep track of:
    --   currently reversed or not
    --   start offset
    --   end offset
    --   frame length

    go :: Bool -> n -> n -> Active n f' a' -> [a']
    go rev offset frameLen (Prim d f) = case d of
      -- XXX fix for rev
      Duration d -> map f . takeWhile (<= d) . map ((offset+) . (/frames)) $ [0 ..]
      Forever    -> map (f . (offset+) . (/frames)) $ [0 ..]
    go rev offset frameLen (Fmap _ f a) = map f (go rev offset frames a)

      -- XXX have to deal with potential overlap! ugh.
      -- XXX fix for rev!
    go rev offset frameLen (Seq _ a1 a2) = go rev offset frames a1 ++ go rev offset' frames a2
      where
        Duration d1 = getDuration a1
        offset'     = frameLen - mod' (d1 - offset) frameLen

        mod' n d = n - fromInteger (floor (n/d)) * d

--    go offset frameLen (SeqR _ a1 a2)
--    go offset frameLen (SeqL _ a1 a2)

    go _ _ _ (Pure a) = repeat a
    go rev offset frameLen (Ap _ af ax)  = undefined
    go rev offset frameLen (Par _ a1 a2) = undefined
    go rev offset frameLen (Rev a)       = go (not rev) offset frameLen a  -- XXX offset?
    go rev offset frameLen (Str _ k a)   = go rev (offset/k) (frameLen/k) a
    go rev offset frameLen (Cut (Duration d) a)     = undefined

--------------------------------------------------
-- Sequential composition

-- $seq
--
-- Composing 'Active' values sequentially means placing them
-- end-to-end so one occurs after the other.  The most basic
-- sequential composition operator is ('->-'), which does exactly
-- that.
--
-- The only nuance is what happens at the precise point of overlap
-- (recall that finite 'Active' values are defined on a /closed/
-- interval).  If using a floating-point duration type, it probably
-- doesn't matter what happens at the precise point of overlap, since
-- the probability of sampling at that exact point is very small; but
-- if using rational durations it might matter quite a bit, since one
-- might sample at a frame rate which evenly divides the durations
-- used in constructing the 'Active', and hence always sample
-- precisely on the points of overlap between primitive 'Active'
-- values.
--
-- The answer is that ('->-') requires a 'Semigroup' instance for the
-- type 'a', and when composing @x ->- y@, the value at the end of @x@
-- will be combined with the value at the start of @y@ using ('<>').
-- If @a@ does not have a 'Semigroup' instance, one can, for example,
-- wrap it in 'Last', so that the value from @x@ will be ignored and
-- the value from @y@ taken. In fact, the ('->>') and ('>>-')
-- operators are provided for convenience which handle this common
-- situation; ('->>') uses the starting value from its right-hand
-- argument at the point of overlap, throwing away the ending value
-- from its left-hand argument (using 'Last'); ('>>-') does the
-- converse (using 'First').
--
-- Finite 'Active' values form a semigroup under horizontal
-- composition as long as the value type @a@ is a 'Semigroup';
-- additionally, if @a@ is a 'Monoid', then finite active values are
-- as well, with @'instant' 'mempty'@ as the identity.  However, the
-- 'Semigroup' and 'Monoid' instances for 'Active' are for parallel
-- rather than sequential composition.  The instances with sequential
-- composition are instead defined for the 'Sequential' newtype
-- wrapper.

infixr 4 ->-, ->>, >>-, -<>-

-- | Sequential composition.
--
--   @x ->- y@ is the active which behaves first as @x@, and then as
--   @y@; the total duration is the sum of the durations of @x@ and
--   @y@.  The value of @x ->- y@ at the instant @x@ and @y@ overlap
--   is the composition of @x@ and @y@'s values under ('<>').
--
--   Note that @x@ must be finite, but @y@ may be infinite.
(->-) :: (Semigroup a, Num n, Ord n) => Active n F a -> Active n f a -> Active n f a
a1 ->- a2 = Seq (addDuration (getDuration a1) (getDuration a2)) a1 a2

-- | Sequential composition, preferring the value from the right-hand
--   argument at the instant of overlap.
--
--   XXX example / (picture)
(->>) :: forall n f a. (Num n, Ord n) => Active n F a -> Active n f a -> Active n f a
a1 ->> a2 = SeqR (addDuration (getDuration a1) (getDuration a2)) a1 a2

-- | Sequential composition, preferring the value from the left-hand
--   argument at the instant of overlap.
(>>-) :: forall n f a. (Num n, Ord n) => Active n F a -> Active n f a -> Active n f a
a1 >>- a2 = SeqL (addDuration (getDuration a1) (getDuration a2)) a1 a2

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
(-<>-) :: (Semigroup a, Fractional n, Ord n) => Active n F a -> Active n f a -> Active n f a
a1 -<>- a2 = a1 ->> ((end a1 <>) <$> a2)

-- | A newtype wrapper for finite 'Active' values.  The 'Semigroup'
--   and 'Monoid' instances for this wrapper use sequential rather
--   than parallel composition (specifically, ('->-')).
newtype Sequential n a = Sequential { getSequential :: Active n F a }

instance (Num n, Ord n, Semigroup a) => Semigroup (Sequential n a) where
  Sequential a1 <> Sequential a2 = Sequential (a1 ->- a2)

instance (Num n, Ord n, Monoid a, Semigroup a) => Monoid (Sequential n a) where
  mempty  = Sequential (instant mempty)
  mappend = (<>)

-- XXX should we have variants of 'movie' and friends that work like ->> ?

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
movie []     = error "Active.movie: Can't make empty movie!"
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
a1 <+> a2 = Par (maxDuration (getDuration a1) (getDuration a2)) a1 a2

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

-- | @'always' x@ creates an infinite 'Active' which is constantly
--   'x'.  A synonym for 'ipure', but with no type class constraints
--   on the numeric type @n@.
always :: a -> Active n I a
always a = activeI (const a)

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
stretch s a
  | s <= 0 = error "Active.stretch: Nonpositive stretch factor.  Use stretch' instead."
  | otherwise = Str (s *^ getDuration a) s a

-- | Like 'stretch', but allows negative stretch factors, which
--   reverse the active.  As a result, it is restricted to only finite
--   actives.
stretch' :: (Fractional n, Ord n) => n -> Active n F a -> Active n F a
stretch' s a
    | s > 0     = stretch s a
    | s < 0     = stretch (abs s) (backwards a)
    | otherwise = error "Active.stretch': stretch factor of 0"


-- | Flip an 'Active' value so it runs backwards.  For obvious
--   reasons, this only works on finite 'Active'\s.
backwards :: Num n => Active n F a -> Active n F a
backwards = Rev

matchDuration :: (Ord n, Fractional n) => Active n F a -> Active n F a -> Active n F a
matchDuration a1 a2 = stretch (d2/d1) a1
  where
    Duration d1 = getDuration a1
    Duration d2 = getDuration a2

stretchTo :: (Ord n,  Fractional n) => n -> Active n F a -> Active n F a
stretchTo n a = stretch (n/d) a
  where
    Duration d = getDuration a

-- | Take a "snapshot" of a given 'Active' at a particular time,
--   freezing the resulting value into an infinite constant.
--
--   @snapshot t a = always (runActive a t)@
snapshot :: (Fractional n, Ord n) => n -> Active n f a -> Active n I a
snapshot t a = always (runActive a t)

-- | @cut d a@ cuts the given 'Active' @a@ to the specified duration
--   @d@.  Has no effect if @a@ is already shorter than @d@.
cut :: (Num n, Ord n) => n -> Active n f a -> Active n F a
cut c a = Cut (Duration c `minDuration` getDuration a) a

--------------------------------------------------

{- Reasons we need/want indexing of Finitude:

   If we want to support infinite Actives, then these functions need
   to know about finitude:

   - end
   - backwards
   - stretchTo
   - matchDuration

-}

----------------------------------------------------------------------
-- diagrams-haddock illustrations.  The code is not included in the
-- typeset documentation.
--
-- > testDia :: Diagram B
-- > testDia = circle 1 # fc blue # frame 0.1

