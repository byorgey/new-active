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
--   relative: one can say that `a` should start two seconds after `b`,
--   but one cannot say that `a` should start at time 20 and `b` at
--   time 22.
-- * 'Active' values can be composed both in sequence and in parallel,
--   with special attention paid to how underlying values should be
--   composed when there is overlap.
--
-- XXX examples/tutorial
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
  , start, end, sample

    -- * Sequential composition
    -- $seq

  , (->-), (->>), (>>-), (-<>-)
  , Sequential(..), stitchNE, stitch, movieNE, movie

    -- * Parallel composition
    -- $par

  , IApplicative(..)

  , parU, (<∪>), stackNE, stack, parI, (<∩>)

    -- * Other combinators

  , stretch, stretch', stretchTo, matchDuration
  , cut, backwards, snapshot

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
--   Example:
--
--   >>> let act = activeF (2*pi) (\d -> sin d + cos d) :: ActF Double
--   >>> let ht x = 8 + round (4*x)
--   >>> mapM_ putStrLn $ sample 4 (act <#> \x -> replicate (ht x) '*')
--   ************
--   *************
--   *************
--   **************
--   **************
--   *************
--   ************
--   ***********
--   **********
--   *********
--   *******
--   ******
--   *****
--   ****
--   ***
--   **
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

activeF :: RealFrac d => d -> (d -> a) -> Active 'F a
activeF d f = Active (Duration (toRational d)) (f . fromRational)

-- | Smart constructor for infinite 'Active' values, given a total
--   function of type \(d \to a\) giving a value of type \(a\) at every
--   time.
activeI :: Fractional d => (d -> a) -> Active 'I a
activeI f = Active Forever (f . fromRational)

-- | Generic smart constructor for 'Active' values, given a 'Duration'
--   and a function on the appropriate interval.
active :: RealFrac d => Duration f d -> (d -> a) -> Active f a
active d f = Active (toRational <$> d) (f . fromRational)

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
--   > uiDia = illustrateActiveR ui
ui :: Active 'F Rational
ui = active 1 id

-- | An infinite sine wave with a period of @1@, that is,
--   \( d \mapsto \sin(2\pi d) \).  This can be convenient when
--   creating repetitive behavior with a period measured in whole
--   number units.
--
--   <<diagrams/src_Active_sin'Dia.svg#diagram=sin'Dia&width=200>>
--
--   > sin'Dia = illustrateActive' 0.1 sin'
sin' :: Floating n => Active 'I n
sin' = dur <#> \n -> sin (2*pi*fromRational n)

-- | An infinite cosine wave with a period of @1@, that is,
--   \( d \mapsto \cos(2\pi d) \).   This can be convenient when
--   creating repetitive behavior with a period measured in whole
--   number units.
--
--   <<diagrams/src_Active_cos'Dia.svg#diagram=cos'Dia&width=200>>
--
--   > cos'Dia = illustrateActive' 0.1 cos'
cos' :: Floating n => Active 'I n
cos' = dur <#> \n -> cos (2*pi*fromRational n)

-- | @interval a b@ varies linearly from \( a \) to \( b \) over a
--   duration of \( b - a \).  That is, it represents the function \( d \mapsto a + d \).
--
--   <<diagrams/src_Active_intervalDia.svg#diagram=intervalDia&width=200>>
--
--   > intervalDia = illustrateActive (interval 1 4)
interval :: RealFrac d => d -> d -> Active 'F d
interval a b = active (toDuration (b - a)) (a+)

-- | @dur@ is the infinite active value representing the function
--   \( d \mapsto d \).  It is called @dur@ since it can be thought of as
--   representing "the current duration" at any point in time.
--
--   <<diagrams/src_Active_durDia.svg#diagram=durDia&width=200>>
--
--   > durDia = illustrateActiveR dur
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
--   > pamfDia = illustrateActive' 0.1 . fmap getSum $
--   >   interval 0 3 <#> Sum `parI` sin' <#> Sum

(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip (<$>)

-- | Create a "discrete" 'Active' from a nonempty list of values.  The
--   resulting 'Active' has duration 1, and takes on each value from
--   the list in turn for a duration of \(1/n\), where \(n\) is the
--   number of items in the list.
--
--   XXX picture --- show step function
--
--   If you want the result to last longer than 1 unit, you can use
--   'stretch'.
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

runActive :: Real d => Active f a -> (d -> a)
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

runActiveMay :: Real d => Active f a -> (d -> Maybe a)
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
runActiveOpt :: Real d => Active f a -> (d -> Option a)
runActiveOpt a = Option . runActiveMay a

-- | Do a case analysis on an 'Active' value of unknown finitude,
--   doing one thing if it is finite and another if it is infinite.
--
--   As an example, the @makeFinite@ function defined below leaves the
--   duration of finite actives alone, but cuts infinite actives down
--   to have a default duration of 3.
--
--   >>> let makeFinite :: Active f a -> ActF a; makeFinite = withActive id (cut 3)
--   >>> sample 1 (makeFinite (lasting 7 'a'))
--   "aaaaaaaa"
--   >>> sample 1 (makeFinite (always 'a'))
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
start :: Active f a -> a
start (Active _ f) = f 0

-- | Extract the value at the end of a finite 'Active'.
end :: Active 'F a -> a
end (Active (Duration d) f) = f d

-- | Generate a list of "frames" or "samples" taken at regular
--   intervals from an 'Active' value.  The first argument is the
--   "frame rate", or number of samples per unit time.  That is,
--   @sample f a@ samples @a@ at times
--   \( 0, \frac 1 f, \frac 2 f, \dots \),
--   ending at the last multiple of \(1/f\) which is not
--   greater than the duration.  The list will be infinite iff the
--   'Active' is.
--
--   >>> sample 2 (interval 0 3 <#> (^2) :: ActF Double)
--   [0.0,0.25,1.0,2.25,4.0,6.25,9.0]
sample :: Real d => d -> Active f a -> [a]
sample 0  _ = error "Active.sample: Frame rate can't equal zero"
sample fr (Active (Duration d) f) = map f . takeWhile (<= d) . map (/toRational fr) $ [0 ..]

  -- We'd like to just say (map f [0, 1/n .. d]) above but that
  -- doesn't work, because of the weird behavior of Enum with floating
  -- point: the last element of the list might actually be a bit
  -- bigger than d.  This way we also avoid the error that can
  -- accumulate by repeatedly adding 1/n.

sample fr (Active Forever      f) = map (f . (/toRational fr)) $ [0 ..]

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
(->-) :: Semigroup a => Active 'F a -> Active f a -> Active f a
(Active d1@(Duration n1) f1) ->- (Active d2 f2) = Active (addDuration d1 d2) f
  where
    f n | n <  n1   = f1 n
        | n == n1   = f1 n <> f2 0
        | otherwise = f2 (n - n1)

-- | Sequential composition, preferring the value from the right-hand
--   argument at the instant of overlap.
--
--   XXX example / (picture)
(->>) :: forall f a. Active 'F a -> Active f a -> Active f a
a1 ->> a2 = coerce ((coerce a1 ->- coerce a2) :: Active f (Last a))

-- | Sequential composition, preferring the value from the left-hand
--   argument at the instant of overlap.
(>>-) :: forall f a. Active 'F a -> Active f a -> Active f a
a1 >>- a2 = coerce ((coerce a1 ->- coerce a2) :: Active f (First a))

-- | Accumulating sequential composition.
--
--   @x -<>- y@ first behaves as @x@, and then behaves as @y@, except
--   that the final value of @x@ is combined via ('<>') with every
--   value from @y@.  For example:
--
--   >>> :m +Data.Ratio
--   >>> let x = active 3 Sum :: ActF (Sum Rational)
--   >>> let a1 = x ->- x
--   >>> let a2 = x -<>- x
--   >>> map (numerator . getSum) (sample 1 a1)
--   [0,1,2,3,1,2,3]
--   >>> map (numerator . getSum) (sample 1 a2)
--   [0,1,2,3,4,5,6]
--
--   @(-<>-)@ satisfies the law:
--
--   @x -<>- y = x ->> (('end' x '<>') '<$>' y)@
(-<>-) :: Semigroup a => Active 'F a -> Active f a -> Active f a
(Active d1@(Duration n1) f1) -<>- (Active d2 f2) = Active (addDuration d1 d2) f
  where
    f n | n < n1    = f1 n
        | otherwise = f1 n1 <> f2 (n - n1)

-- | A newtype wrapper for finite 'Active' values.  The 'Semigroup'
--   and 'Monoid' instances for this wrapper use sequential rather
--   than parallel composition (specifically, ('->-')).
newtype Sequential a = Sequential { getSequential :: Active 'F a }

instance Semigroup a => Semigroup (Sequential a) where
  Sequential a1 <> Sequential a2 = Sequential (a1 ->- a2)

instance (Monoid a, Semigroup a) => Monoid (Sequential a) where
  mempty  = Sequential (instant mempty)
  mappend = (<>)

-- XXX should we have variants of 'movie' and friends that work like
-- ->> ?  YES, we should!  Otherwise you e.g. get weird "blips" when
-- diagrams are superimposed with each other.  Maybe this should be the default?

-- Combinator for combining an (infinite) background with finite stuff.

-- | "Stitch" a nonempty list of finite actives together, via ('->-'),
--   so values are combined via the 'Semigroup' instance at the splice
--   points.  (See also 'movieNE'.)  Uses a balanced fold which can be
--   more efficient than the usual linear fold.
stitchNE :: Semigroup a => NonEmpty (Active 'F a) -> Active 'F a
stitchNE = getSequential . foldB1 . coerce

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

-- | A variant of 'stitchNE' defined on lists instead of 'NonEmpty'
--   for convenience; @stitch []@ is a runtime error.
stitch :: Semigroup a => [Active 'F a] -> Active 'F a
stitch []     = error "Active.stitch: Can't make empty stitch!"
stitch (a:as) = stitchNE (a :| as)

-- | Make a "movie" out of a nonempty list of finite actives,
--   sequencing them one after another via ('->>'), so the value of
--   the right-hand 'Active' is taken at each splice point.
movieNE :: forall a. NonEmpty (Active 'F a) -> Active 'F a
movieNE scenes = coerce (stitchNE (coerce scenes :: NonEmpty (Active 'F (Last a))))

-- | A variant of 'movieNE' defined on lists instead of 'NonEmpty' for
--   convenience; @movie []@ is a runtime error.
movie :: forall a. [Active 'F a] -> Active 'F a
movie []     = error "Active.movie: Can't make empty movie!"
movie scenes = coerce (stitch (coerce scenes :: [Active 'F (Last a)]))

--------------------------------------------------
-- Parallel composition

-- $par
-- This is a paragraph about parallel composition.

----------------------------------------
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

-- | If @a@ is a 'Semigroup', then 'Active n f a' forms a 'Semigroup'
--   under unioning parallel composition.  Notice that the two
--   arguments of ('<>') are restricted to be either both finite or
--   both infinite; ('<⊔>') is strictly more general since it can
--   combine active values with different finitudes.
instance Semigroup a => Semigroup (Active f a) where
  (<>) = (<∪>)

-- | If @a@ is a 'Monoid', then 'Active n F a' forms a 'Monoid' under
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

----------------------------------------
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

-- XXX  don't export
-- XXX comment
unsafeStretch :: Rational -> Active f a -> Active f a
unsafeStretch s (Active d f) = Active (s *^ d) (f . (/s))

-- | Stretch an active value by a positive factor.  For example,
--   @'stretch' 2 a@ is twice as long as @a@ (and hence half as fast).
--   Conversely, @'stretch' (1/2) a@ is half as long as @a@, and hence
--   twice as fast.
--
--   This actually works perfectly well on infinite active values,
--   despite the fact that it no longer makes sense to say that the
--   result is "x times as long" as the input; it simply stretches out
--   the values along the number line.
stretch :: Real d => d -> Active f a -> Active f a
stretch s a
  | s <= 0    = error "Active.stretch: Nonpositive stretch factor.  Use stretch' instead."
  | otherwise = unsafeStretch s' a
  where
    s' = toRational s

-- | Like 'stretch', but allows negative stretch factors, which
--   reverse the active.  As a result, it is restricted to only finite
--   actives.
stretch' :: Real d => d -> Active 'F a -> Active 'F a
stretch' s a
  | s' > 0     = unsafeStretch s' a
  | s' < 0     = unsafeStretch (abs s') (backwards a)
  | otherwise = error "Active.stretch': stretch factor of 0"
  where
    s' = toRational s

-- | Flip an 'Active' value so it runs backwards.  For obvious
--   reasons, this only works on finite 'Active'\s.
backwards :: Active 'F a -> Active 'F a
backwards (Active (Duration d) f) =  Active (Duration d) (f . (d-))

-- XXX comment me
matchDuration :: Active 'F a -> Active 'F a -> Active 'F a
matchDuration a@(Active (Duration d1) _) (Active (Duration d2) _) = stretch (d2/d1) a

-- XXX comment me
stretchTo :: Real d => d -> Active 'F a -> Active 'F a
stretchTo n a@(Active (Duration d) _) = stretch (toRational n / d) a

-- | Take a "snapshot" of a given 'Active' at a particular time,
--   freezing the resulting value into an infinite constant.
--
--   @snapshot t a = always (runActive a t)@
snapshot :: Real d => d -> Active f a -> Active 'I a
snapshot t a = always (runActive a t)

-- | @cut d a@ cuts the given 'Active' @a@ to the specified duration
--   @d@.  Has no effect if @a@ is already shorter than @d@.
cut :: Real d => d -> Active f a -> Active 'F a
cut c (Active d f) = Active ((Duration $ toRational c) `minDuration` d) f

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
-- > illustrateActiveSum :: Active f (Sum Double) -> Diagram B
-- > illustrateActiveSum = illustrateActive . fmap getSum
-- >
-- > illustrateActiveR :: Active f Rational -> Diagram B
-- > illustrateActiveR = illustrateActive . fmap fromRational
-- >
-- > illustrateActive :: Active f Double -> Diagram B
-- > illustrateActive = illustrateActive' (1/2)
-- >
-- > illustrateActive' :: Rational -> Active f Double -> Diagram B
-- > illustrateActive' pd = frame 0.5 . withActive (endPt <> base) base
-- >   where
-- >     endPt act
-- >       = closedPt
-- >         # moveTo (fromRational (durationF act) ^& end act)
-- >     base :: Active f Double -> Diagram B
-- >     base act = mconcat
-- >       [ closedPt # moveTo (0 ^& start act)
-- >       , zipWith (^&) (map fromRational [0, pd ..])
-- >                      (sample (1/pd) (act # cut 5.5))
-- >         # cubicSpline False
-- >         # lc red
-- >       , axes
-- >       ]
-- >
-- > closedPt = circle 0.15 # lw none  # fc red
-- > openPt   = circle 0.15 # lw thick # lc red
