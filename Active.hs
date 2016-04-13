-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Active
-- Copyright   :  
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  
--
-- Inspired by the work of Kevin Matlage and Andy Gill (/Every/
-- /Animation Should Have a Beginning, a Middle, and an End/, Trends
-- in Functional Programming,
-- 2010. <http://ittc.ku.edu/csdl/fpg/node/46>), this module defines a
-- simple abstraction for working with time-varying values.  A value
-- of type @Active a@ is either a constant value of type @a@, or a
-- time-varying value of type @a@ (/i.e./ a function from time to
-- @a@) with specific start and end times.  Since active values
-- have start and end times, they can be aligned, sequenced,
-- stretched, or reversed.
--
-- In a sense, this is sort of like a stripped-down version of
-- functional reactive programming (FRP), without the reactivity.
--
-- The original motivating use for this library is to support making
-- animations with the diagrams framework
-- (<http://projects.haskell.org/diagrams>), but the hope is that it
-- may find more general utility.
--
-- There are two basic ways to create an @Active@ value.  The first is
-- to use 'mkActive' to create one directly, by specifying a start and
-- end time and a function of time.  More indirectly, one can use the
-- 'Applicative' instance together with the unit interval 'ui', which
-- takes on values from the unit interval from time 0 to time 1, or
-- 'interval', which creates an active over an arbitrary interval.
--
-- For example, to create a value of type @Active Double@ which
-- represents one period of a sine wave starting at time 0 and ending
-- at time 1, we could write
--
-- > mkActive 0 1 (\t -> sin (fromTime t * tau))
--
-- or
--
-- > (sin . (*tau)) <$> ui
--
-- 'pure' can also be used to create @Active@ values which are
-- constant and have no start or end time.  For example,
--
-- > mod <$> (floor <$> interval 0 100) <*> pure 7
--
-- cycles repeatedly through the numbers 0-6.
--
-- Note that the \"idiom bracket\" notation supported by the SHE
-- preprocessor (<http://personal.cis.strath.ac.uk/~conor/pub/she/>,
-- <http://hackage.haskell.org/package/she>) can make for somewhat
-- more readable 'Applicative' code.  For example, the above example
-- can be rewritten using SHE as
--
-- > {-# OPTIONS_GHC -F -pgmF she #-}
-- >
-- > ... (| mod (| floor (interval 0 100) |) ~7 |)
--
-- There are many functions for transforming and composing active
-- values; see the documentation below for more details.
--
--
-- With careful handling, this module should be suitable to generating
-- deep embeddings if 'Active' values.
--
-----------------------------------------------------------------------------

module Data.Active
       ( -- * Representing durations of time

         -- ** Time and duration

         Time, toTime, fromTime
       , Duration, toDuration, fromDuration

         -- * Dynamic values
       , Dynamic(..), mkDynamic, onDynamic

       , shiftDynamic

         -- * Active values
         -- $active
       , Active, mkActive, fromDynamic, isConstant, isDynamic

       , onActive, modActive, runActive

       , activeEra, setEra, atTime

       , activeStart, activeEnd

         -- * Combinators

         -- ** Special active values

       , ui, interval

         -- ** Transforming active values

       , stretch, stretchTo, during
       , shift, backwards

       , snapshot

         -- ** Working with values outside the era
       , clamp, clampBefore, clampAfter
       , trim, trimBefore, trimAfter

         -- ** Composing active values

       , after

       , (->>)

       , (|>>), movie

         -- * Discretization

       , discrete
       , simulate

       ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

import           Control.Arrow       ((&&&))
import           Control.Lens        hiding (backwards, (<.>))

import           Data.Functor.Apply
import           Data.Maybe
import           Data.Monoid         (First (..))
import           Data.Monoid.Inf
import           Data.Semigroup      hiding (First (..))
import qualified Data.Vector         as V

import           Linear
import           Linear.Affine


------------------------------------------------------------
-- Time
------------------------------------------------------------

-- | An abstract type representing a finite /elapsed time/ between two points
--   in time.  Note that durations can be negative. Literal numeric
--   values may be used as @Duration@s thanks to the 'Num' and
--   'Fractional' instances.
data Duration n = Forever | Duration n
  -- deriving (Eq, Ord, Show, Read, Enum, Num, Fractional, Real, RealFrac, Functor)

-- | A convenient wrapper function to convert a numeric value into a duration.
toDuration :: n -> Duration n
toDuration = Duration

-- | A convenient unwrapper function to turn a duration into a numeric value.
fromDuration :: Duration n -> Maybe n
fromDuration Forever = Nothing
fromDuration (Duration n) = Just n

instance Num n => Semigroup (Duration n) where
  Forever <> _ = Forever
  _ <> Forever = Forever
  Duration a <> Duration b = Duration (a + b)

instance Num n => Monoid (Duration n) where
  mappend = (<>)
  mempty  = Duration 0

------------------------------------------------------------
--  Active
------------------------------------------------------------

data Active f n a = Finite (Duration n) (Duration n -> a)
  deriving (Functor)

-- | Sequence/overlay two 'Active' values: shift the second to start
--   immediately after the first (using 'after'), then compose them
--   (using '<>').
(->>) :: Semigroup a => Active a -> Active a -> Active a
a1 ->> a2 = a1 <> (a2 `after` a1)


-- XXX illustrate

-- | \"Splice\" two 'Active' values together: shift the second to
--   start immediately after the first (using 'after'), and produce
--   the value which acts like the first up to the common end/start
--   point, then like the second after that.  If both are constant,
--   return the first.
(|>>) :: Active a -> Active a -> Active a
a1 |>> a2 = (fromJust . getFirst) <$>
            (trimAfter (First . Just <$> a1) ->> trimBefore (First . Just <$> a2))

-- XXX implement 'movie' with a balanced fold

-- | Splice together a list of active values using '|>>'.  The list
--   must be nonempty.
movie :: [Active a] -> Active a
movie = foldr1 (|>>)

------------------------------------------------------------
--  Discretization
------------------------------------------------------------

-- | Create an @Active@ which takes on each value in the given list in
--   turn during the time @[0,1]@, with each value getting an equal
--   amount of time.  In other words, @discrete@ creates a \"slide
--   show\" that starts at time 0 and ends at time 1.  The first
--   element is used prior to time 0, and the last element is used
--   after time 1.
--
--   It is an error to call @discrete@ on the empty list.
discrete :: [a] -> Active a
discrete [] = error "Data.Active.discrete must be called with a non-empty list."
discrete xs = f <$> ui
  where f (t :: Rational)
            | t <= 0    = V.unsafeHead v
            | t >= 1    = V.unsafeLast v
            | otherwise = V.unsafeIndex v $ floor (t * fromIntegral (V.length v))
        v = V.fromList xs

-- | @simulate r act@ simulates the 'Active' value @act@, returning a
--   list of \"snapshots\" taken at regular intervals from the start
--   time to the end time.  The interval used is determined by the
--   rate @r@, which denotes the \"frame rate\", that is, the number
--   of snapshots per unit time.
--
--   If the 'Active' value is constant (and thus has no start or end
--   times), a list of length 1 is returned, containing the constant
--   value.
simulate :: Rational -> Active a -> [a]
simulate 0    = const []
simulate rate =
  onActive (:[])
           (\d -> map (runDynamic d)
                      (let s = start (era d)
                           e = end   (era d)
                       in  [s, s + 1^/rate .. e]
                      )
           )

