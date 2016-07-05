{-# LANGUAGE DataKinds #-}

import           Text.Printf

import qualified Animation                   as A
import           Control.IApplicative
import           Control.Monad               (forM_)
import           Data.Active
import           Data.Finitude

import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude            hiding (interval, simulate, ui, (->>),
                                             backwards, stretchTo, stretch,
                                             runActive, snapshot, movie, imap)


background :: A.Animation Double I Rasterific V2 Double
background = (ipure (rect 6 6 # fc white))

------------------
movingCircle' :: A.Animation Double F Rasterific V2 Double
movingCircle' = (\t -> circle 0.25 # fc blue # translateX t) <:$> interval 0 1

movingCircleV ::A.Animation Double F Rasterific V2 Double
movingCircleV = (\t -> circle 0.25 # fc green # translateY t) <:$> interval 0 1

movingCircle2 :: A.Animation Double F Rasterific V2 Double
movingCircle2 = (<>) <:$> movingCircle' <:*> ipure (rect 4 2 # translateX 1 # fc white)

------------------

sinCircle :: A.Animation Double F Rasterific V2 Double
sinCircle =
  (<>) <:$> (translateX <:$> (sin <:$> interval 0 (0.5*pi)) <:*> (ipure (circle 1 # fc blue)))
       <:*> ipure (rect 4 2 # fc white)

circleCircle :: A.Animation Double F Rasterific V2 Double
circleCircle =
  (<>) <:$> (translateX <:$> (sin <:$> interval 0 (2*pi)) <:*> (translateY <:$> (cos <:$> interval 0 (2*pi)) <:*> (ipure (circle 0.25 # fc red))))
  <:*> background

------------------

------------------

movingCircle :: A.Animation Double F Rasterific V2 Double
movingCircle = (\t -> circle 0.25 # fc green # translateX t) <:$> interval 0 2

circleTriangle :: A.Animation Double F Rasterific V2 Double
circleTriangle =  (<>) <:$> (movingCircle 
        ->> movingCircle # rotate (120 @@ deg) # translateX 2
        ->> movingCircle # rotate (240 @@ deg) # translateX 1 # translateY 1.732) <:*> background

------------------

-- Examples of functions made

------------------
-- backwards
backwardsEx :: A.Animation Double F Rasterific V2 Double
backwardsEx =  circleCircle ->> backwards circleCircle 

------------------
-- stretchTo
stretchToEx :: A.Animation Double F Rasterific V2 Double
stretchToEx = stretchTo (pi) circleCircle

------------------
-- stretch
stretchEx :: A.Animation Double F Rasterific V2 Double
stretchEx = stretch (2) movingCircle2

------------------
-- matchDuration
matchDurationEx :: A.Animation Double F Rasterific V2 Double
matchDurationEx = matchDuration collision stretchEx

------------------
-- snapshot
snapshotEx :: A.Animation Double I Rasterific V2 Double
snapshotEx = snapshot (2) circleTriangle

------------------
-- truncateDuration
truncateDurationEx :: A.Animation Double F Rasterific V2 Double
truncateDurationEx = truncateDuration circleCircle stretchEx
-- is this correct?

------------------
-- (->>)
movingCircleRight :: A.Animation Double F Rasterific V2 Double
movingCircleRight = (<>) <:$> movingCircle' <:*> background

movingCircleUP :: A.Animation Double F Rasterific V2 Double
movingCircleUP = (<>) <:$> movingCircleV <:*> background

arrowsEx :: A.Animation Double F Rasterific V2 Double
arrowsEx = movingCircleRight ->> (movingCircleUP # translateX 2)

arrowsEx2 :: A.Animation Double F Rasterific V2 Double
arrowsEx2 = (<>) <:$> (movingCircle' ->> 
          (movingCircle' # rotate (90 @@ deg) # translateX  1)) <:*> background

------------------
-- runActive
--runActiveEx :: A.Animation Double F Rasterific V2 Double
--runActiveEx = runActive circleCircle (0.5)

------------------
-- movie
movieEx :: A.Animation Double F Rasterific V2 Double
movieEx = movie [arrowsEx, arrowsEx2] -- only works for two elements

------------------
-- stack
movingCircleY :: A.Animation Double F Rasterific V2 Double
movingCircleY = (translateY <:$> (sin <:$> interval 0 (2*pi)) <:*> (ipure (circle 0.25 # fc yellow)))

movingCircleYO :: A.Animation Double F Rasterific V2 Double
movingCircleYO = (translateY <:$> ((\x -> - sin x) <:$> interval 0 (2*pi)) <:*> (ipure (circle 0.25 # fc purple)))

movingCircle1 :: A.Animation Double F Rasterific V2 Double
movingCircle1 = (translateX <:$> (sin <:$> interval 0 (2*pi)) <:*> (ipure (circle 0.25 # fc blue)))

movingCircleO1 :: A.Animation Double F Rasterific V2 Double
movingCircleO1 = (translateX <:$> ((\x -> - sin x) <:$> interval 0 (2*pi)) <:*> (ipure (circle 0.25 # fc green)))

collision :: A.Animation Double F Rasterific V2 Double
collision = (<>) <:$> stack [movingCircleY, movingCircleYO, movingCircle1, movingCircleO1] <:*> background

-- TODO: make examples for ui, interval, dur, startVal, endVal, fix runActiveEx, discrete, fix snapshotEx
------------------
-- <:>
example :: A.Animation Double F Rasterific V2 Double
example = circleCircle <:> circleTriangle <:> collision
-- compiles and runs, but not correctly!

------------------
-- a1 <> a2
example2 :: A.Animation Double F Rasterific V2 Double
example2 = circleCircle <> circleTriangle <> collision
-- produced same animation as example, talk about diference between them

------------------
-- imap example?

------------------
main :: IO ()
main = do
  let frames = simulate 30 example2
  forM_ (zip [0 :: Int ..] frames) $ \(i,frame) -> do
    renderRasterific (printf "out/frame%03d.png" i) (mkWidth 400) frame

{-

ghc --make Test && ./Test && cd out && ffmpeg -i frame%03d.png -vcodec mpeg4 test.mov && open test.mov && cd ..
  
ghc --make Test && rm -f out/*.png &&  ./Test && cd out && ffmpeg -i frame%03d.png -vcodec mpeg4 test.mov && open test.mov && cd ..



f v1 v2 v3   -- function f applied to single values v1, v2, v3

-- now suppose v1, v2, v3 are *animations*:

f <:$> v1 <:*> v2 <:*> v3

-}
