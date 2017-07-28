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
-- Some of the important concepts/features of the library include:
--
-- * Every 'Active' value has a /duration/, which is either a
--   nonnegative rational number, or infinity.
-- * An 'Active' value with duration
--   \(d\) can be thought of as a function \([0,d] \to a\), assigning a
--   value to each instant on the closed interval \([0,d]\).
-- * 'Active' values are /time-invariant/, that is, they do not have a
--   fixed, absolute starting time.  Put another way, time is always
--   relative: one can say that `a` should start two seconds after `b`
--   but one cannot say that `a` should start at 2pm on Thursday.
-- * 'Active' values can be composed both in sequence and in parallel,
--   with special attention paid to how underlying values should be
--   composed when there is overlap.
--
-- XXX examples/tutorial --- where?
--
-----------------------------------------------------------------------------

module Active
  ( -- * Durations
    -- | A few things are re-exported from the "Active.Duration"
    --   module for convenience.

    Finitude(..), Duration(..), toDuration

    -- * The Active type
  , Active, ActF, ActI

    -- * Primitives
  , activeF, activeI, active
  , instant, lasting, always
  , ui, interval, dur
  , sin', cos'
  , (<#>)
  , discreteNE, discrete

    -- * Running/sampling

  , runActive, runActiveMay, runActiveOpt
  , withActive
  , duration, durationF
  , start, end, samples

    -- * Sequential composition
    -- $seq

    -- ** Stitching
  , (->-), stitchNE, stitch, Sequential(..)

    -- ** Movies
  , (->>), (>>-), movieNE, movie


    -- ** Accumulating
  , (-<>-), accumulateNE, accumulate

    -- * Parallel composition
    -- $par

  , IApplicative(..)

  , parU, (<∪>), stackNE, stack, parI, (<∩>)

    -- * Other combinators

  , stretch, stretch', stretchTo, matchDuration
  , cut, omit, slice, backwards, snapshot

  ) where

import           Data.Coerce

import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import           Data.Maybe           (fromJust)
import           Data.Semigroup
import qualified Data.Vector          as V
import           Linear.Vector

import           Active.Duration
import           Control.IApplicative


-- XXX go through and include diagrams of everything!

------------------------------------------------------------
--  Active
------------------------------------------------------------

-- | A value of type @Active f a@ is a time-varying value of type
--   @a@ with a given duration.
--
--   * @f@ is an index indicating whether the duration is finite or
--   infinite.
--   * @a@ is the type of the values.
--
--   If the duration is infinite, it can be thought of as a function
--   \( [0,+\infty) \to a \); if it has a finite duration \( d \), it
--   can be thought of as a function \( [0,d] \to a \) (note in
--   particular that the interval is /closed/ on both ends: the
--   function is defined at \(0\) as well as at the duration \(d\)).
--
--   @Active f@ is a @Functor@, and @Active@ is an 'IApplicative';
--   if @a@ is a 'Semigroup' then @Active f a@ is as well.  These
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
-- @
-- activeF 3 (\d -> if d <= 3 then d*2 else error "o noes!")
-- @
--
--   is considered a well-defined, total 'Active' value, even though
--   the provided Haskell function is partial.  Because 'Active' is
--   abstract, it is impossible to ever observe the value of an
--   'Active' past its duration.
--
-- @
-- >> let a = activeF 3 (\d -> if d <= 5 then d*2 else error "o noes!")
-- >> runActive a 4
-- *** Exception: Active.runActive: Active value evaluated past its duration.
-- @
--
--   Even though in this example the provided Haskell function is
--   defined at the value 4 (in particular it is equal to 8), it is
--   impossible to observe this since the 'Active' has a duration of
--   only 3.

data Active :: Finitude -> * -> * where
  Active   :: Duration f Rational -> (Rational -> a) -> Active f a
  deriving Functor

-- | The type of finite 'Active' values; a convenient synonym for
--   @Active 'F@, so you can use the library without having to turn on
--   @DataKinds@.
type ActF = Active 'F

-- | The type of infinite 'Active' values; a convenient synonym for
--   @Active 'I@, so you can use the library without having to turn on
--   @DataKinds@.
type ActI = Active 'I

--------------------------------------------------
-- Constructing

-- | Smart constructor for finite 'Active' values, given a finite
--   numeric duration \(d\) and a function from \([0,d] \to a\).
--
--   @'activeF' d f = ('cut' d 'dur')    '<#>' f
--            = ('interval' 0 d) '<#>' f@
--
--   In the example below, @activeF 2 (^2)@ constructs the Active
--   value which lasts for 2 time units and takes on the value
--   \( t^2 \) at time \( t \).
--
--   <<diagrams/src_Active_activeFDia.svg#diagram=activeFDia&width=200>>
--
--   > activeFDia = illustrateActive $ activeF 2 (^2)

activeF :: Rational -> (Rational -> a) -> Active 'F a
activeF d = Active (Duration d)

-- | Smart constructor for infinite 'Active' values, given a total
--   function of type \(d \to a\) giving a value of type \(a\) at every
--   time.
--
--   <<diagrams/src_Active_activeIDia.svg#diagram=activeIDia&width=200>>
--
--   > activeIDia = illustrateActive' 0.1 [] $ activeI (sqrt . fromRational)
activeI :: (Rational -> a) -> Active 'I a
activeI = Active Forever

-- | Generic smart constructor for 'Active' values, given a 'Duration'
--   and a function on the appropriate interval.
active :: Duration f Rational -> (Rational -> a) -> Active f a
active = Active

-- | A value of duration zero.
--
--   @'instant' a = 'lasting' 0 a@
--
--   <<diagrams/src_Active_instantDia.svg#diagram=instantDia&width=200>>
--
--   > instantDia = illustrateActive (instant 2)

instant :: a -> Active 'F a
instant = lasting 0

-- | A constant value lasting for the specified duration.
--
--   This reads particularly nicely when used with postfix function
--   application, a la @(#)@ from the diagrams library.  For example:
--
--   @
-- c :: ActF Char
-- c = movie
--   [ 'a' # lasting 2
--   , 'b' # lasting 3
--   , 'c' # lasting 1
--   ]
-- @
--
-- @'lasting' d = 'activeF' d . const
--          = 'cut' d . always@
--
--   <<diagrams/src_Active_lastingDia.svg#diagram=lastingDia&width=200>>
--
--   > lastingDia = illustrateActive (2 # lasting 3)

lasting :: Rational -> a -> Active 'F a
lasting d = activeF d . const

-- | The unit interval: the identity function on the interval \( [0,1] \).
--
--   <<diagrams/src_Active_uiDia.svg#diagram=uiDia&width=200>>
--
--   > uiDia = illustrateActive ui
ui :: Active 'F Rational
ui = active 1 id

-- | An infinite sine wave with a period of @1@, that is,
--   \( d \mapsto \sin(2\pi d) \).  This can be convenient when
--   creating repetitive behavior with a period measured in whole
--   number units.
--
--   <<diagrams/src_Active_sin'Dia.svg#diagram=sin'Dia&width=200>>
--
--   > sin'Dia = illustrateActive' 0.1 [] sin'
--
--   >>> let act = cut 1 ((+) <$> sin' <:*> cos')
--   >>> let ht x = 8 + round (4*x)
--   >>> mapM_ putStrLn $ samples 24 (act <#> \x -> replicate (ht x) '*')
--   ************
--   *************
--   *************
--   **************
--   *************
--   *************
--   ************
--   ***********
--   *********
--   ********
--   *******
--   *****
--   ****
--   ***
--   ***
--   **
--   ***
--   ***
--   ****
--   *****
--   *******
--   ********
--   *********
--   ***********
--   ************

sin' :: Floating n => Active 'I n
sin' = dur <#> \n -> sin (2*pi*fromRational n)

-- | An infinite cosine wave with a period of @1@, that is,
--   \( d \mapsto \cos(2\pi d) \).   This can be convenient when
--   creating repetitive behavior with a period measured in whole
--   number units.
--
--   <<diagrams/src_Active_cos'Dia.svg#diagram=cos'Dia&width=200>>
--
--   > cos'Dia = illustrateActive' 0.1 [] cos'
cos' :: Floating n => Active 'I n
cos' = dur <#> \n -> cos (2*pi*fromRational n)

-- | @interval a b@ varies linearly from \( a \) to \( b \) over a
--   duration of \( |a - b| \).  That is, it represents the function \( d \mapsto a + d \)
--   if \( a \leq b \), and \( d \mapsto a - d \) otherwise.
--
--   <<diagrams/src_Active_intervalDia.svg#diagram=intervalDia&width=200>>
--
--   > intervalDia = illustrateActive (interval 1 4)
interval :: Rational -> Rational -> Active 'F Rational
interval a b
  | a <= b    = active (toDuration (b - a)) (a+)
  | otherwise = active (toDuration (a - b)) (a-)

-- | @dur@ is the infinite active value representing the function
--   \( d \mapsto d \).  It is called @dur@ since it can be thought of as
--   representing "the current duration" at any point in time.
--
--   <<diagrams/src_Active_durDia.svg#diagram=durDia&width=200>>
--
--   > durDia = illustrateActive dur
dur :: Active 'I Rational
dur = active Forever fromRational

infixl 8 <#>

-- | Backwards 'fmap', that is, a synonym for @'flip' ('<$>')@.  This
--   can be useful when starting from some 'Active' like 'ui',
--   'interval', or 'dur', and then applying a function to it. For
--   example:
--
--   @interval 3 5 '<#>' \\t -> circle 1 # translateX t@
--
--   produces a circle translated continuously from 3 to 5 along the
--   x-axis.
--
--   ('<#>') has the same precedence as ('#') from the diagrams
--   library (namely, @infixl 8@) for the same reason: so an 'Active'
--   build via ('<#>') can be combined with others via infix
--   combinators such as 'parI' without needing parentheses.
--
--   <<diagrams/src_Active_pamfDia.svg#diagram=pamfDia&width=200>>
--
--   > pamfDia = illustrateActive' 0.1 [] . fmap getSum $
--   >   interval 0 3 <#> (Sum . fromRational) `parI` sin' <#> Sum

(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip (<$>)

-- | Create a "discrete" 'Active' from a nonempty list of values.  The
--   resulting 'Active' has duration 1, and takes on each value from
--   the list in turn for a duration of \(1/n\), where \(n\) is the
--   number of items in the list.  As illustrated below, each interval
--   is "open on the right", that is, at the precise moment when
--   switching from one value to the next, the second value is taken.
--
--   See also 'discrete', which takes a list instead of a 'NonEmpty'.
--
--   If you want the result to last longer than 1 unit, you can use
--   'stretch'.
--
--   <<diagrams/src_Active_discreteNEDia.svg#diagram=discreteNEDia&width=200>>
--
--   > import Data.List.NonEmpty (NonEmpty(..))
--   > discreteNEDia = illustrateActive' (1/2) [(4/3,OC),(8/3,OC)]
--   >   (discreteNE (1 :| [2,3]) # stretch 4)

discreteNE :: NonEmpty a -> Active 'F a
discreteNE (a :| as) = (Active 1 f)
  where
    f t
      | t == 1    = V.unsafeLast v
      | otherwise = V.unsafeIndex v $ floor (t * fromIntegral (V.length v))
    v = V.fromList (a:as)

-- | Like 'discreteNE', but with a list for convenience.  Calling
--   'discrete' on the empty list is a runtime error.
discrete :: [a] -> Active 'F a
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
--
--   >>> let act = movie [lasting 2 "hello", lasting 3 "world"] :: ActF String
--   >>> runActive act 1
--   "hello"
--   >>> runActive act 4
--   "world"

runActive :: Active f a -> (Rational -> a)
runActive (Active d f) t
  = case compareDuration (Duration t') d of
      GT -> error "Active.runActive: Active value evaluated past its duration."
      _  -> f t'
  where
    t' = toRational t

-- | Like 'runActive', but return a total function that returns
--   @Nothing@ when queried outside its range.
--
--   >>> let act = movie [lasting 2 "hello", lasting 3 "world"] :: ActF String
--   >>> runActiveMay act 1
--   Just "hello"
--   >>> runActiveMay act 4
--   Just "world"
--   >>> runActiveMay act 6
--   Nothing

runActiveMay :: Active f a -> (Rational -> Maybe a)
runActiveMay (Active d f) t
  = case compareDuration (Duration t') d of
      GT -> Nothing
      _  -> Just (f t')
  where
    t' = toRational t

-- | Like 'runActiveMay', but return an 'Option' instead of 'Maybe'.
--   Sometimes this is more convenient since the 'Monoid' instance for
--   'Option' only requires a 'Semigroup' constraint on its type
--   argument.
runActiveOpt :: Active f a -> (Rational -> Option a)
runActiveOpt a = Option . runActiveMay a

-- | Do a case analysis on an 'Active' value of unknown finitude,
--   doing one thing if it is finite and another if it is infinite.
--
--   As an example, the @makeFinite@ function defined below leaves the
--   duration of finite actives alone, but cuts infinite actives down
--   to have a default duration of 3.
--
--   >>> let makeFinite :: Active f a -> ActF a; makeFinite = withActive id (cut 3)
--   >>> samples 1 (makeFinite (lasting 7 'a'))
--   "aaaaaaaa"
--   >>> samples 1 (makeFinite (always 'a'))
--   "aaaa"

withActive :: (Active 'F a -> b) -> (Active 'I a -> b) -> Active f a -> b
withActive onFinite onInfinite a@(Active d f) =
  case d of
    Duration _ -> onFinite a
    Forever    -> onInfinite a

-- | Extract the duration of an 'Active' value.  Returns 'Nothing' for
--   infinite values.
--
--   >>> duration (lasting 3 'a')
--   Just (3 % 1)
--   >>> duration (movie [lasting 3 'a', lasting 2 'b'])
--   Just (5 % 1)
--   >>> duration (always 'a')
--   Nothing
duration :: Active f a -> Maybe Rational
duration (Active d _) = fromDuration d

-- | Extract the duration of an 'Active' value that is known to be
--   finite.
--
--   >>> durationF (lasting 3 'a')
--   3 % 1
--   >>> durationF (movie [lasting 3 'a', lasting 2 'b'])
--   5 % 1
durationF :: Active F a -> Rational
durationF (Active d _) = fromDurationF d

-- | Extract the value at the beginning of an 'Active'.
--
--   >>> start (always 3)
--   3
--   >>> start (omit 2 (stretch 3 dur))
--   2 % 3
start :: Active f a -> a
start (Active _ f) = f 0

-- | Extract the value at the end of a finite 'Active'.
--
--   >>> end (activeF 3 (^2))
--   9 % 1
--   >>> end ui
--   1 % 1
--   >>> end (cut 3 $ movie [lasting 1 'a', lasting 3 'b', lasting 2 'c'])
--   'b'

end :: Active 'F a -> a
end (Active (Duration d) f) = f d

-- | Generate a list of "frames" or "samples" taken at regular
--   intervals from an 'Active' value.  The first argument is the
--   "frame rate", or number of samples per unit time.  That is,
--   @samples f a@ samples @a@ at times
--   \( 0, \frac 1 f, \frac 2 f, \dots \),
--   ending at the last multiple of \(1/f\) which is not
--   greater than the duration.  The list of samples will be infinite
--   iff the 'Active' is.
--
--   >>> samples 2 (interval 0 3 <#> (^2))
--   [0 % 1,1 % 4,1 % 1,9 % 4,4 % 1,25 % 4,9 % 1]
--   >>> samples 1 (lasting 3 ())
--   [(),(),(),()]
--   >>> samples 1 (lasting 2.9 ())
--   [(),(),()]
samples :: Rational -> Active f a -> [a]
samples 0  _ = error "Active.samples: Frame rate can't equal zero"
samples fr (Active (Duration d) f) = map f . takeWhile (<= d) . map (/toRational fr) $ [0 ..]

  -- We'd like to just say (map f [0, 1/n .. d]) above but that
  -- doesn't work, because of the weird behavior of Enum with floating
  -- point: the last element of the list might actually be a bit
  -- bigger than d.  This way we also avoid the error that can
  -- accumulate by repeatedly adding 1/n.

samples fr (Active Forever      f) = map (f . (/toRational fr)) $ [0 ..]

------------------------------------------------------------
-- Sequential composition
------------------------------------------------------------

-- $seq
--
-- Composing 'Active' values sequentially means placing them
-- end-to-end so one occurs after the other.  The most basic
-- sequential composition operator is ('->-'), which does exactly
-- that.
--
-- The only nuance is what happens at the precise point of overlap
-- (recall that finite 'Active' values are defined on a /closed/
-- interval).  If one were to use @Double@ values as durations, it
-- probably wouldn't matter what happened at the precise point of
-- overlap, since the probability of sampling at that exact point
-- would be very small.  But since we are using rational durations, it
-- matters quite a bit, since one might reasonably sample at a frame
-- rate which evenly divides the durations used in constructing the
-- 'Active', and hence end up sampling precisely on the points of
-- overlap between primitive 'Active' values.
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

--------------------------------------------------
-- Stitching

-- | "Stitching" sequential composition.
--
--   @x ->- y@ is the active which behaves first as @x@, and then as
--   @y@; the total duration is the sum of the durations of @x@ and
--   @y@.  The value of @x ->- y@ at the instant @x@ and @y@ overlap
--   is the composition of @x@ and @y@'s values with respect to the
--   underlying 'Semigroup', that is, they are combined with ('<>').
--
--   Note that @x@ must be finite, but @y@ may be infinite.
--
--   See also 'stitchNE' and 'stitch', which combine an entire list of
--   'Active' values using this operator, and the 'Sequential'
--   wrapper, which has 'Semigroup' and 'Monoid' instances
--   corresponding to ('->-').
--
--   <<diagrams/src_Active_seqMDia.svg#diagram=seqMDia&width=200>>
--
--   > seqMDia = illustrateActive' (1/4) [(2,OO)]
--   >   ((fromRational . getSum) <$> (interval 0 2 <#> Sum ->- always 3 <#> Sum))
--
--   In the above example, the values at the endpoints of the two
--   actives are combined via summing.  This particular example is a
--   bit silly---it is hard to imagine wanting this behavior.  Here
--   are some examples of cases where this combining behavior might be
--   more desirable:
--
--   * Imagine making an animation in which one diagram disappears
--     just as another one appears. We might explicitly want a single
--     frame at the splice point with /both/ diagrams visible (since,
--     /e.g./, this might make the transition look better/more
--     continuous to the human eye).
--
--   * We can use the 'Max' or 'Min' semigroups to automatically pick
--     the larger/smaller of two values at a splice point, no matter
--     whether it comes from the left or the right.
--
--       <<diagrams/src_Active_seqMMaxDia.svg#diagram=seqMMaxDia&width=200>>
--
--       > seqMMaxDia = illustrateActive' (1/4) [(1,CO),(2,OC)] $ getMax <$>
--       >   (lasting 1 (Max 3) ->- lasting 1 (Max 2) ->- lasting 1 (Max 4))
--
--   More generally, although explicit uses of this combinator may be
--   less common than ('->>') or ('>>-'), it is more fundamental:
--   those combinators are implemented in terms of this one, via the
--   'Last' and 'First' semigroups.

(->-) :: Semigroup a => Active 'F a -> Active f a -> Active f a
(Active d1@(Duration n1) f1) ->- (Active d2 f2) = Active (addDuration d1 d2) f
  where
    f n | n <  n1   = f1 n
        | n == n1   = f1 n <> f2 0
        | otherwise = f2 (n - n1)

-- | A newtype wrapper for finite 'Active' values.  The 'Semigroup'
--   and 'Monoid' instances for this wrapper use stitching sequential
--   composition (that is, ('->-')) rather than parallel composition.
newtype Sequential a = Sequential { getSequential :: Active 'F a }

instance Semigroup a => Semigroup (Sequential a) where
  Sequential a1 <> Sequential a2 = Sequential (a1 ->- a2)

instance (Monoid a, Semigroup a) => Monoid (Sequential a) where
  mempty  = Sequential (instant mempty)
  mappend = (<>)
  mconcat [] = mempty
  mconcat ss = Sequential (stitch (coerce ss))

-- | "Stitch" a nonempty list of finite actives together, via ('->-'),
--   so values are combined via the 'Semigroup' instance at the splice
--   points.  (See also 'movieNE' and 'stitch'.)  Uses a balanced fold which can be
--   more efficient than the usual linear fold.
stitchNE :: Semigroup a => NonEmpty (Active 'F a) -> Active 'F a
stitchNE = getSequential . foldB1 . coerce

-- | A variant of 'stitchNE' defined on lists instead of 'NonEmpty'
--   for convenience; @stitch []@ is a runtime error.
--
--   <<diagrams/src_Active_stitchDia.svg#diagram=stitchDia&width=200>>
--
--   > stitchDia = illustrateActive' (1/4) [(1,CO),(2,OC),(3,OC),(4,CO)] $ getMax <$>
--   >   (stitch . map (fmap Max . lasting 1)) [3,1,4,5,2]

stitch :: Semigroup a => [Active 'F a] -> Active 'F a
stitch []     = error "Active.stitch: Can't make empty stitch!"
stitch (a:as) = stitchNE (a :| as)

--------------------------------------------------
-- Movies

-- | Sequential composition, preferring the value from the right-hand
--   argument at the instant of overlap.
--
--   <<diagrams/src_Active_seqRDia.svg#diagram=seqRDia&width=200>>
--
--   > seqRDia = illustrateActive' (1/4) [(2,OC)]
--   >   (fromRational <$> (interval 0 2 ->> always 3))
--
--   >>> samples 1 (cut 4 (interval 0 2 ->> always 3))
--   [0 % 1,1 % 1,3 % 1,3 % 1,3 % 1]
(->>) :: forall f a. Active 'F a -> Active f a -> Active f a
a1 ->> a2 = coerce ((coerce a1 ->- coerce a2) :: Active f (Last a))

-- | Sequential composition, preferring the value from the left-hand
--   argument at the instant of overlap.
--
--   <<diagrams/src_Active_seqLDia.svg#diagram=seqLDia&width=200>>
--
--   > seqLDia = illustrateActive' (1/4) [(2,CO)]
--   >   (fromRational <$> (interval 0 2 >>- always 3))
--
--   >>> samples 1 (cut 4 (interval 0 2 >>- always 3))
--   [0 % 1,1 % 1,2 % 1,3 % 1,3 % 1]
(>>-) :: forall f a. Active 'F a -> Active f a -> Active f a
a1 >>- a2 = coerce ((coerce a1 ->- coerce a2) :: Active f (First a))

-- | Make a "movie" out of a nonempty list of finite actives,
--   sequencing them one after another via ('->>'), so the value of
--   the right-hand 'Active' is taken at each splice point.  See also
--   'movie'.
movieNE :: forall a. NonEmpty (Active 'F a) -> Active 'F a
movieNE scenes = coerce (stitchNE (coerce scenes :: NonEmpty (Active 'F (Last a))))

-- | A variant of 'movieNE' defined on lists instead of 'NonEmpty' for
--   convenience; @movie []@ is a runtime error.
--
--   <<diagrams/src_Active_movieDia.svg#diagram=movieDia&width=200>>
--
--   > {-# LANGUAGE TupleSections #-}
--   > movieDia = illustrateActive' (1/4) (map (,OC) [1..4]) $ fromRational <$>
--   >   movie [lasting 1 3, lasting 1 2, interval 0 2 # stretch 0.5, lasting 1 4]
movie :: forall a. [Active 'F a] -> Active 'F a
movie []     = error "Active.movie: Can't make empty movie!"
movie scenes = coerce (stitch (coerce scenes :: [Active 'F (Last a)]))

--------------------------------------------------
-- Accumulating

-- | Accumulating sequential composition.
--
--   @x -<>- y@ first behaves as @x@, and then behaves as @y@, except
--   that the final value of @x@ is combined via ('<>') with every
--   value from @y@.  So the final value of @x@ "accumulates" into the
--   next 'Active' value being composed.
--
--   See also 'accumulateNE' and 'accumulate' which perform
--   accumulating sequential composition of a whole list.
--
--   An example of a situation where this is useful is in putting
--   together a sequence of geometric transformations of an object
--   following some path through space.  If we want the object to move
--   right one unit, then move up one unit, and so on, we can achieve
--   this via ('-<>-') (or 'accumulate'): after finishing its
--   rightward motion we don't want it to jump back to the origin and
--   then start moving up; we want the effect of the rightward motion
--   to persist, /i.e./ accumulate into the subsequent translations.
--
--   @x -<>- y = x ->> (('end' x '<>') '<$>' y)@
--
--   <<diagrams/src_Active_seqADia.svg#diagram=seqADia&width=200>>
--
--   > seqADia = illustrateActive' (0.1) [(1.5,CC),(3,CC)]
--   >   ((fromRational . getSum) <$> (stair -<>- stair -<>- stair))
--   >   where
--   >     stair = (ui ->> lasting 0.5 1) <#> Sum
--
--   >>> :m +Data.Ratio
--   >>> let x = active 3 Sum :: ActF (Sum Rational)
--   >>> let a1 = x ->- x
--   >>> let a2 = x -<>- x
--   >>> map (numerator . getSum) (samples 1 a1)
--   [0,1,2,3,1,2,3]
--   >>> map (numerator . getSum) (samples 1 a2)
--   [0,1,2,3,4,5,6]
--
(-<>-) :: Semigroup a => Active 'F a -> Active f a -> Active f a
(Active d1@(Duration n1) f1) -<>- (Active d2 f2) = Active (addDuration d1 d2) f
  where
    f n | n < n1    = f1 n
        | otherwise = f1 n1 <> f2 (n - n1)

-- | A newtype wrapper for finite 'Active' values.  The 'Semigroup'
--   and 'Monoid' instances for this wrapper use accumulating
--   sequential composition (that is, ('-<>-') rather than parallel
--   composition.
newtype Accumulating a = Accumulating { getAccumulating :: Active 'F a }

instance Semigroup a => Semigroup (Accumulating a) where
  Accumulating a1 <> Accumulating a2 = Accumulating (a1 ->- a2)

instance (Monoid a, Semigroup a) => Monoid (Accumulating a) where
  mempty  = Accumulating (instant mempty)
  mappend = (<>)
  mconcat [] = mempty
  mconcat ss = Accumulating (accumulate (coerce ss))

-- | "Accumulate" a nonempty list of finite actives together in
--   sequence, via ('-<>-'), so the end value from each component
--   'Active' accumulates into the next.  Uses a balanced fold which
--   can be more efficient than the usual linear fold.
accumulateNE :: Semigroup a => NonEmpty (Active 'F a) -> Active 'F a
accumulateNE = getAccumulating . foldB1 . coerce

-- | A variant of 'accumulateNE' defined on lists instead of 'NonEmpty'
--   for convenience; @accumulate []@ is a runtime error.
--
--   XXX example
accumulate :: Semigroup a => [Active 'F a] -> Active 'F a
accumulate []     = error "Active.accumulate: Can't accumulate empty list!"
accumulate (a:as) = accumulateNE (a :| as)

------------------------------------------------------------
-- Parallel composition
------------------------------------------------------------

-- $par
-- In addition to sequential composition (with one 'Active' following
-- another in time), 'Active'\s also support /parallel/ composition,
-- where two or more 'Active'\s happen simultaneously.  There are two
-- main types of parallel composition, with the difference coming down
-- to how a parallel composition between 'Active'\s of different
-- durations is handled.  /Unioning/ parallel composition ('parU')
-- results in an 'Active' as long as its longest input; conversely,
-- /intersecting/ parallel composition ('parI') results in an 'Active'
-- as long as its shortest input.
--
-- XXX why we want/need both and can't really define one in terms of
-- the other.  Generalizing 'parI' to 'Applicative' instance, in which
-- case we really need the intersecting.

--------------------------------------------------
-- Unioning parallel composition

infixr 6 <∪>
infixr 6 `parU`

-- | Unioning parallel composition.  The duration of @x \`parU\` y@ is the
--   /maximum/ of the durations of @x@ and @y@.  Where they are both
--   defined, the values are combined with ('<>').  Where only one is
--   defined, its value is simply copied.
parU :: Semigroup a => Active f1 a -> Active f2 a -> Active (f1 ⊔ f2) a
a1@(Active d1 _) `parU` a2@(Active d2 _)
  = Active (d1 `maxDuration` d2)
           (\t -> fromJust . getOption $ runActiveOpt a1 t <> runActiveOpt a2 t)
                  -- fromJust is safe since the (Nothing, Nothing) case
                  -- can't happen: at least one of a1 or a2 will be defined everywhere
                  -- on the interval between 0 and the maximum of their durations.

-- | An infix Unicode synonym for 'parU'.
(<∪>) :: Semigroup a => Active f1 a -> Active f2 a -> Active (f1 ⊔ f2) a
(<∪>) = parU

-- | If @a@ is a 'Semigroup', then 'Active f a' forms a 'Semigroup'
--   under unioning parallel composition.  Notice that the two
--   arguments of ('<>') are restricted to be either both finite or
--   both infinite; ('parU') is strictly more general since it can
--   combine active values with different finitudes.
instance Semigroup a => Semigroup (Active f a) where
  (<>) = (<∪>)

-- | If @a@ is a 'Monoid', then 'Active F a' forms a 'Monoid' under
--   unioning parallel composition.  The identity element is
--   @'instant' 'mempty'@, the same as the identity element for the
--   sequential composition monoid (see 'Sequential').
instance (Monoid a, Semigroup a) => Monoid (Active 'F a) where
  mempty = instant mempty
  mappend = (<>)

-- | \"Stack\" a nonempty list of active values via unioning parallel
--   composition.  This is actually just a synonym for 'sconcat'.
stackNE :: Semigroup a => NonEmpty (Active f a) -> Active f a
stackNE = sconcat

-- | Like 'stackNE', but on a list for convenience.  Calling 'stack'
--   on the empty list is a runtime error.
stack :: Semigroup a => [Active f a] -> Active f a
stack = sconcat . NE.fromList

--------------------------------------------------
-- Intersecting parallel composition

instance IFunctor Active where
  imap f (Active d1 g) = Active d1 (f . g)

-- | @'Active'@ is an 'IApplicative', somewhat akin to 'ZipList':
--
--   * 'ipure' creates an infinite constant value.
--   * @f '<:*>' x@ applies @f@ to @x@ pointwise, taking the minimum
--     duration of @f@ and @x@.
instance IApplicative Active where
  type Id = 'I
  type (:*:) i j = i ⊓ j
  ipure = always
  Active d1 f1 <:*> Active d2 f2 = Active (d1 `minDuration` d2) (f1 <*> f2)

-- | @'always' x@ creates an infinite 'Active' which is constantly
--   'x'.  A synonym for 'ipure'.
--
--   <<diagrams/src_Active_alwaysDia.svg#diagram=alwaysDia&width=200>>
--
--   > alwaysDia = illustrateActive (always 2)

always :: a -> Active 'I a
always = Active Forever . const

infixr 6 `parI`
infixr 6 <∩>

-- | Intersecting parallel composition.  The duration of @x \`parI\` y@ is
--   the /minimum/ of the durations of @x@ and @y@.
--
--   Note that this is a special case of the 'IApplicative' instance
--   for 'Active'; in fact, it is equivalent to @'iliftA2' ('<>')@.
parI :: Semigroup a => Active f1 a -> Active f2 a -> Active (f1 ⊓ f2) a
parI = iliftA2 (<>)

-- | An infix Unicode synonym for 'parI'.
(<∩>) :: Semigroup a => Active f1 a -> Active f2 a -> Active (f1 ⊓ f2) a
(<∩>) = parI

--------------------------------------------------
-- Other combinators

-- | Do the actual stretching of an 'Active' value by a given positive
--   constant.  Unsafe because it does not check whether the constant
--   is positive.  This is not exported, only used to implement
--   'stretch' and 'stretch''.
unsafeStretch :: Rational -> Active f a -> Active f a
unsafeStretch s (Active d f) = Active (s *^ d) (f . (/s))

-- | Stretch an active value by a positive factor.  For example,
--   @'stretch' 2 a@ is twice as long as @a@ (and hence half as fast).
--   Conversely, @'stretch' (1/2) a@ is half as long as @a@ (twice as
--   fast).
--
--   This actually works perfectly well on infinite active values,
--   despite the fact that it no longer makes sense to say that the
--   result is "x times as long" as the input; it simply stretches out
--   the values in time.
--
--   <<diagrams/src_Active_stretchDia.svg#diagram=stretchDia&width=200>>
--
--   > stretchDia = illustrateActive (ui # stretch 3)
--
--   >>> samples 1 (stretch 3 ui)
--   [0 % 1,1 % 3,2 % 3,1 % 1]
--   >>> take 4 (samples 1 (stretch (1/2) dur))
--   [0 % 1,2 % 1,4 % 1,6 % 1]
stretch :: Rational -> Active f a -> Active f a
stretch s a
  | s <= 0    = error "Active.stretch: Nonpositive stretch factor.  Use stretch' instead."
  | otherwise = unsafeStretch s a

-- | Like 'stretch', but allows negative stretch factors, which
--   reverse the active.  As a result, it is restricted to only finite
--   actives.
--
--   <<diagrams/src_Active_stretch'Dia.svg#diagram=stretch'Dia&width=200>>
--
--   > stretch'Dia = illustrateActive (ui # stretch' (-3))
--
--   >>> samples 1 (stretch' (-3) ui)
--   [1 % 1,2 % 3,1 % 3,0 % 1]
stretch' :: Rational -> Active 'F a -> Active 'F a
stretch' s a
  | s > 0     = unsafeStretch s a
  | s < 0     = unsafeStretch (abs s) (backwards a)
  | otherwise = error "Active.stretch': stretch factor of 0"

-- | Flip an 'Active' value so it runs backwards.  For obvious
--   reasons, this only works on finite 'Active'\s.
--
--   <<diagrams/src_Active_backwardsDia.svg#diagram=backwardsDia&width=200>>
--
--   > backwardsDia = illustrateActive (backwards ui)
backwards :: Active 'F a -> Active 'F a
backwards (Active (Duration d) f) =  Active (Duration d) (f . (d-))

-- | Stretch the first active so it has the same duration as the
--   second.
--
--   <<diagrams/src_Active_matchDurationDia.svg#diagram=matchDurationDia&width=200>>
--
--   > matchDurationDia = illustrateActive' 0.1 [ (1.5,CC),(3.5,CC) ] $ getSum <$>
--   >   stack [ base, matchDuration (cut 7 cos') base <#> Sum ]
--   >   where
--   >     base = movie [ stretch 1.5 ui, interval 1 3, interval 3 2 ] <#> (Sum . fromRational)

matchDuration :: Active 'F a -> Active 'F b -> Active 'F a
matchDuration a@(Active (Duration d1) _) (Active (Duration d2) _) = stretch (d2/d1) a

-- | Stretch a finite active by whatever factor is required so that it
--   ends up with the given duration.
--
--   <<diagrams/src_Active_stretchToDia.svg#diagramg=stretchToDia&width=200>>
--
--   > stretchToDia = illustrateActive (interval 0 3 # stretchTo 5)
--
--   >>> durationF (stretchTo 5 (interval 0 3))
--   5 % 1
stretchTo :: Rational -> Active 'F a -> Active 'F a
stretchTo n a@(Active (Duration d) _) = stretch (n / d) a

-- | Take a "snapshot" of a given 'Active' at a particular time,
--   freezing the resulting value into an infinite constant.
--
--   @snapshot t a = always (runActive a t)@
snapshot :: Rational -> Active f a -> Active 'I a
snapshot t a = always (runActive a t)

-- | @cut d a@ cuts the given 'Active' @a@ to the specified duration
--   @d@.  Has no effect if @a@ is already shorter than @d@.
--
--   <<diagrams/src_Active_cutDia.svg#diagram=cutDia&width=200>>
--
--   > cutDia = illustrateActive' 0.1 [] (cut 1.7 cos')
cut :: Rational -> Active f a -> Active 'F a
cut c (Active d f) = Active (Duration c `minDuration` d) f

-- | @omit d a@ omits the first @d@ time units from @a@. The result is
--   only defined if @d@ is less than or equal to the duration of @a@.
--
--   <<diagrams/src_Active_omitDia.svg#diagram=omitDia&width=200>>
--
--   > omitDia = illustrateActive (omit 1.3 (interval 0 3))
omit :: Rational -> Active f a -> Active f a
omit o (Active d f) = Active (d `subDuration` (Duration o)) (f . (+o))

-- XXX
slice :: Rational -> Rational -> Active f a -> Active 'F a
slice s e = cut (e - s) . omit s

--------------------------------------------------

--------------------------------------------------
-- Utilities

-- | A balanced binary fold.
foldB1 :: Semigroup a => NonEmpty a -> a
foldB1 (a :| as) = maybe a (a <>) (foldBM as)
  where
    foldBM :: Semigroup a => [a] -> Maybe a
    foldBM = getOption . foldB (<>) (Option Nothing) . map (Option . Just)

    foldB :: (a -> a -> a) -> a -> [a] -> a
    foldB _   z []   = z
    foldB _   _ [x]  = x
    foldB (&) z xs   = foldB (&) z (pair (&) xs)

    pair _   []         = []
    pair _   [x]        = [x]
    pair (&) (x:y:zs) = (x & y) : pair (&) zs

----------------------------------------------------------------------
-- diagrams-haddock illustrations.  The code is not included in the
-- typeset documentation.
--
-- > axes :: Diagram B
-- > axes = mconcat
-- >   [ hashes
-- >   , hashes # rotateBy (1/4)
-- >   , arrowV (5.5 *^ unitX)
-- >   , arrowV (5.5 *^ unitY)
-- >   ]
-- >   # lw thick
-- >   where
-- >     hashes = atPoints (iterateN 5 (translateX 1) (1 ^& 0)) (repeat (vrule 0.15))
-- >
-- > illustrateActiveSum :: RealFrac d => Active f (Sum d) -> Diagram B
-- > illustrateActiveSum = illustrateActive . fmap getSum
-- >
-- > illustrateActive :: RealFrac d => Active f d -> Diagram B
-- > illustrateActive = illustrateActive' (1/2) [] . fmap realToFrac
-- >
-- > type Discontinuity = (Rational, DiscontinuityType)
-- > data DiscontinuityType = OC | CO | OO | CC | N
-- >
-- > illustrateActive' :: Rational -> [Discontinuity] -> Active f Double -> Diagram B
-- > illustrateActive' pd discs = frame 0.5 . withActive (endPt <> base) base
-- >   where
-- >     endPt act
-- >       = closedPt
-- >         # moveTo (fromRational (durationF act) ^& end act)
-- >     base :: Active f Double -> Diagram B
-- >     base act = foldMap drawSegment segments <> axes
-- >       where
-- >         portionToIllustrate = act # cut 5.5
-- >         discs' = (0,OC) : discs ++ [(durationF portionToIllustrate, N)]
-- >         segments = zip discs' (tail discs')
-- >         drawSegment ((s,d1), (e,d2)) = mconcat [endpts, spline]
-- >           where
-- >             eps = 1/100
-- >             s' = case d1 of { CO -> s + eps; OO -> s + eps; _ -> s }
-- >             e' = case d2 of { OC -> e - eps; OO -> e - eps; _ -> e }
-- >             act' = slice s' e' act
-- >             spline =
-- >               ( zipWith (^&) (map fromRational [s, s + pd ..]) (samples (1/pd) act')
-- >                 ++ [fromRational e ^& end act']
-- >               )
-- >               # cubicSpline False
-- >               # lc red
-- >             endpts = mconcat
-- >               [ (case d1 of CO -> openPt ; OO -> openPt ; _ -> closedPt)
-- >                   # moveTo (fromRational s ^& start act')
-- >               , (case d2 of N -> mempty ; OC -> openPt ; OO -> openPt ; _ -> closedPt)
-- >                   # moveTo (fromRational e ^& end act')
-- >               , (case d1 of OO -> closedPt ; _ -> mempty)
-- >                   # moveTo (fromRational s ^& runActive act s)
-- >               ]
-- >
-- > closedPt, openPt :: Diagram B
-- > closedPt = circle 0.1 # lw none  # fc red
-- > openPt   = circle 0.1 # lc red   # fc white
-- >
