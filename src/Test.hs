{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import           Text.Printf

import qualified Animation                   as A
import           Control.IApplicative
import           Control.Monad               (forM_)
import           Data.Active
import           Data.Finitude

import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude            hiding (interval, simulate, ui, (->>),
                                             backwards, stretchTo, stretch,
                                             runActive, snapshot, movie, imap, Active)


background :: A.Animation Double I Rasterific V2 Double
background = (ipure (rect 55 35 # fc white))

orbit :: (Floating n, Ord n, N a ~ n, Additive (V a), Transformable a, R2 (V a)) 
       => n -> n -> n -> Active n I a -> Active n I a
orbit xT yT sp a = 
           (translateX <:$> ((\c -> xT * cos (sp*c)) <:$> dur) <:*>
           (translateY <:$> ((\c -> yT * sin (sp*c)) <:$> dur) <:*>
            a )  )
           
orbitEx :: A.Animation Double I Rasterific V2 Double
orbitEx = orbit 2 3 4 jupiter

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
arrowsEx = (<>) <:$> (movingCircle'->- movingCircleV) <:*> background

arrowsEx1 :: A.Animation Double F Rasterific V2 Double
arrowsEx1 = (<>) <:$> (movingCircle' ->> 
          (movingCircle' # rotate (90 @@ deg) # translateX  1)) <:*> background

xTrans :: Active Double F (T2 Double)
xTrans = translationX <:$> interval 0 1

yTrans :: Active Double F (T2 Double)
yTrans = translationY <:$> interval 0 1

arrowsEx2 :: A.Animation Double F Rasterific V2 Double
arrowsEx2 = transform <:$> (xTrans ->- yTrans) <:*> ipure(circle 0.25 # fc blue) <:> background
------------------
-- runActive
--runActiveEx :: A.Animation Double F Rasterific V2 Double
--runActiveEx = runActive circleCircle (2)

------------------
-- movie
movieEx :: A.Animation Double F Rasterific V2 Double
movieEx = movie [arrowsEx, arrowsEx2, circleCircle] -- only works for two elements

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
combine :: A.Animation Double F Rasterific V2 Double
combine = circleCircle2 <:> collision

example3 :: A.Animation Double F Rasterific V2 Double
example3 = circleCircle <:> collision

------------------
-- a1 <> a2
example2 :: A.Animation Double F Rasterific V2 Double
example2 = circleCircle <> circleTriangle <> collision


------------------
-- imap example?


------------------
-- overlapping circle
circleCircle2 :: A.Animation Double F Rasterific V2 Double
-- Double: time, F: time, RAsterific: backend rendering, V2: 2D diagrams Double: displacement
circleCircle2 =
  (translateX <:$> (cos <:$> interval 0 (4*pi)) <:*> (translateY <:$> 
  (sin <:$> interval 0 (4*pi)) <:*> (ipure (circle 0.25 # fc purple))))
  
circleCircle2M :: A.Animation Double F Rasterific V2 Double
circleCircle2M =
  (translateX <:$> ((\x ->  2 * cos x) <:$> interval 0 (4*pi)) <:*> (translateY <:$> 
  ((\x -> 1 * sin x) <:$> interval 0 (4*pi)) <:*> (ipure (circle 0.15 # fc green))))  

circleCircleSun :: A.Animation Double F Rasterific V2 Double
circleCircleSun =
 (translateX <:$> ((\x -> - cos x/2) <:$> interval 0 (4*pi)) <:*> 
 (translateY <:$> ((\x -> - sin x/2) <:$> interval 0 (4*pi)) <:*> (ipure (circle 0.5 # fc orange))))

rotating :: A.Animation Double F Rasterific V2 Double
rotating = (<>) <:$> stack [circleCircle2, circleCircleSun, circleCircle2M] <:*> background

----------------
-- overlapping rectangles
blackRect :: A.Animation Double F Rasterific V2 Double
blackRect = (\t -> rect 2 2 # fc black # translateX t) <:$> interval 0 2

movingRect :: A.Animation Double F Rasterific V2 Double
movingRect = (<>) <:$> blackRect <:*> ipure (rect 4 2 # translateX 1 # fc white)

------------------
-- dur 
durEx :: A.Animation Double I Rasterific V2 Double
durEx =    (translateX <:$> ((\x -> cos (x/2)) <:$> dur) <:*> (translateY <:$> 
  ((\x -> cos (x/2))<:$> dur) <:*> (ipure (circle 0.25 # fc black))) <:> background)
  
------------------
-- cut
cutEx :: A.Animation Double F Rasterific V2 Double
cutEx = cut (3) solarSystem


------------------
-- solar system
earth :: A.Animation Double I Rasterific V2 Double
earth = ipure(circle 0.5 # fc blue)

moon :: A.Animation Double I Rasterific V2 Double
moon =    (translateX <:$> ((\x -> 0.7 * cos (6.2*x)) <:$> dur) <:*> (translateY <:$> 
  ((\x -> 0.7 * sin (3*x/2))<:$> dur) <:*> (ipure (circle 0.1 # fc green # lc red))))

moon2 :: A.Animation Double I Rasterific V2 Double
moon2 =    (translateX <:$> ((\x -> 0.7 * cos (5.2*x)) <:$> dur) <:*> (translateY <:$> 
  ((\x -> 0.7 * sin (2*x))<:$> dur) <:*> (ipure (circle 0.1 # fc green # lc yellow)))) 

mercury :: A.Animation Double I Rasterific V2 Double
mercury = ipure(circle 0.25 # fc pink)

venus :: A.Animation Double I Rasterific V2 Double
venus = ipure(circle 0.45 # fc purple)

mars :: A.Animation Double I Rasterific V2 Double
mars = ipure(circle 0.35 # fc deeppink)

jupiter :: A.Animation Double I Rasterific V2 Double
jupiter = ipure(circle 1 # fc peachpuff)

saturn :: A.Animation Double I Rasterific V2 Double
saturn = ipure(circle 0.85 # fc navajowhite)

uranus :: A.Animation Double I Rasterific V2 Double
uranus = ipure(circle 0.7 # fc deepskyblue)

neptune :: A.Animation Double I Rasterific V2 Double
neptune = ipure(circle 0.65 # fc dodgerblue)

pluto :: A.Animation Double I Rasterific V2 Double
pluto = ipure(circle 0.15 # fc yellow # lc pink)
  
sun :: A.Animation Double I Rasterific V2 Double
sun =
 (translateX <:$> ((\x -> - cos x/4) <:$> dur) <:*> 
 (translateY <:$> ((\x -> - sin x/4) <:$> dur) <:*> (ipure (circle 1.5 # fc orange))))
 
sun2 :: A.Animation Double I Rasterific V2 Double
sun2 = (translateX (- 0.5) <:$> ipure(circle 1.4 # fc orange))

earthwM :: A.Animation Double I Rasterific V2 Double
earthwM = (<>) <:$> earth <:*> moon

marswM :: A.Animation Double I Rasterific V2 Double
marswM =(<>) <:$> mars <:*> moon2 

solarSystem2 :: A.Animation Double I Rasterific V2 Double
solarSystem2 = (orbit 2.15  2.15  8.0  mercury) <:>
               (orbit 3.70  2.65  6.7    venus) <:>
               (orbit 5.80  3.50  6.0  earthwM) <:>
               (orbit 8.00  4.45  5.0   marswM) <:>
               (orbit 12.2  5.75  4.0  jupiter) <:>
               (orbit 16.2  7.35  3.0   saturn) <:>
               (orbit 20.5  9.20  2.0   uranus) <:>
               (orbit 24.0  10.7  1.0  neptune) <:>
               (orbit 27.0  12.5  0.8    pluto) <:> sun2 -- <:> background

solarSystem :: A.Animation Double I Rasterific V2 Double
solarSystem = (translateX <:$> ((\x -> (2.15) * cos (8*x)) <:$> dur) <:*>
         (translateY <:$> ((\x -> (2.15) * sin (8*x)) <:$> dur) <:*>
         (mercury)) ) <:>
         
         (translateX <:$> ((\x -> (3.70) * cos (6.7*x)) <:$> dur) <:*> 
         (translateY <:$> ((\x -> (2.65) * sin (6.7*x)) <:$> dur) <:*> 
         (venus))  ) <:>
         
         (translateX <:$> ((\x -> (5.80) * cos (6*x)) <:$> dur) <:*> 
         (translateY <:$> ((\x -> (3.50) * sin (6*x)) <:$> dur) <:*> 
         (earthwM) ) <:>
         
         (translateX <:$> ((\x -> (8.00) * cos (5*x)) <:$> dur) <:*>
         (translateY <:$> ((\x -> (4.45) * sin (5*x)) <:$> dur) <:*>
         (marswM)  ) <:>
         
         (translateX <:$> ((\x -> (12.2) * cos (4*x)) <:$> dur) <:*>
         (translateY <:$> ((\x -> (5.75) * sin (4*x)) <:$> dur) <:*>
         (jupiter)) ) <:>
         
         (translateX <:$> ((\x -> (16.2) * cos (3*x)) <:$> dur) <:*>
         (translateY <:$> ((\x -> (7.35) * sin (3*x)) <:$> dur) <:*>
         (saturn))  ) <:>
         
         (translateX <:$> ((\x -> (20.5) * cos (2*x)) <:$> dur) <:*>
         (translateY <:$> ((\x -> (9.20) * sin (2*x)) <:$> dur) <:*>
         (uranus))  ) <:>
         
         (translateX <:$> ((\x -> (24.0) * cos (1*x)) <:$> dur) <:*>
         (translateY <:$> ((\x -> (10.7) * sin (1*x)) <:$> dur) <:*>
         (neptune)) ) <:>
         
         (translateX <:$> ((\x -> (27.0) * cos (0.8*x)) <:$> dur) <:*>
         (translateY <:$> ((\x -> (12.5) * sin (0.8*x)) <:$> dur) <:*>
         (pluto))  ) <:> sun2 <:> background) )

 
------------------
main :: IO ()
main = do
 {- let frames = simulate 25 (cut 10 solarSystem2)
  forM_ (zip [0 :: Int ..] frames) $ \(i,frame) -> do
    renderRasterific (printf "out/frame%03d.png" i) (mkWidth 400) frame -}
  pic <- loadImageEmb "stars.jpg"
  case pic of
    Left st -> putStrLn st
    Right img -> do
        let background2 ::  Diagram B
            background2 =  image img # sized (mkWidth 55) 
            frames = simulate 25 (cut 20  solarSystem2 <:> ipure(background2)) 
        forM_ (zip [0 :: Int ..] frames) $ \(i,frame) -> do
          renderRasterific (printf "out/frame%03d.png" i) (mkWidth 400) frame

{-

ghc --make Test && ./Test && cd out && ffmpeg -i frame%03d.png -vcodec mpeg4 test.mov && open test.mov && cd ..
  
ghc --make Test && rm -f out/*.png &&  ./Test && cd out && ffmpeg -i frame%03d.png -vcodec mpeg4 test.mov && open test.mov && cd ..



f v1 v2 v3   -- function f applied to single values v1, v2, v3

-- now suppose v1, v2, v3 are *animations*:

f <:$> v1 <:*> v2 <:*> v3

-}
