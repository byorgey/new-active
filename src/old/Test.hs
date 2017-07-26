{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import           Text.Printf
import           Control.Arrow ((&&&))

import qualified Animation                   as A
import           Control.IApplicative
import           Control.Monad               (forM_)
import           Data.Active
import           Data.Finitude

import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude            hiding (interval, simulate, ui, (->>),
                                             backwards, stretchTo, stretch,
                                             runActive, snapshot, movie, imap, Active, (<->))

type Anim f = A.Animation Double f Rasterific V2 Double

background :: Anim I
background = always (rect 55 35 # fc white)

orbit :: (Floating n, Ord n, N a ~ n, Additive (V a), Transformable a, R2 (V a))
       => n -> n -> n -> Active n I a -> Active n I a
orbit xT yT sp a =
  (translateX <:$> ((\c -> xT * cos (sp*c)) <:$> dur) <:*>
  (translateY <:$> ((\c -> yT * sin (sp*c)) <:$> dur) <:*>
  a))

orbitEx :: Anim I
orbitEx = orbit 2 3 4 jupiter

------------------
movingCircle' :: Anim F
movingCircle' =
  -- (\t -> circle 0.25 # fc blue # translateX t) <:$> interval 0 1
  translateX
  <:$> interval 0 1
  <:*> always (circle 0.25 # fc blue)

movingCircleV :: Anim F
movingCircleV =
  translateY
  <:$> interval 0 1
  <:*> always (circle 0.25 # fc green)

movingCircle2 :: Anim F
movingCircle2 = movingCircle' <-> always (rect 4 2 # translateX 1 # fc white)

-- ------------------

sinCircle :: Anim F
sinCircle =
  ( translateX
    <:$> (sin <:$> interval 0 (0.5*pi))
    <:*> ipure (circle 1 # fc blue)
  )

  <-> always (rect 4 2 # fc white)

circleCircle :: Anim F
circleCircle =
  ( translate
    <:$> ((r2 . (sin &&& cos)) <:$> interval 0 (2*pi))
    <:*> ipure (circle 0.25 # fc red)
  )

  <-> background

------------------

movingCircle :: Anim F
movingCircle =
  translateX
  <:$> interval 0 2
  <:*> ipure (circle 0.25 # fc green)

circleTriangle :: Anim F
circleTriangle =
  movie
  [ movingCircle
  , movingCircle # rotateBy (1/3) # translateX 2
  , movingCircle # rotateBy (2/3) # translate (1 ^& 1.732)
  ]

  <-> background

------------------

-- Examples of functions made

------------------
-- backwards
backwardsEx :: Anim F
backwardsEx =  circleCircle ->> backwards circleCircle

------------------
-- stretchTo
stretchToEx :: Anim F
stretchToEx = stretchTo pi circleCircle

------------------
-- stretch
stretchEx :: Anim F
stretchEx = stretch 2 movingCircle2

------------------
-- matchDuration
matchDurationEx :: Anim F
matchDurationEx = matchDuration collision stretchEx

------------------
-- snapshot
snapshotEx :: Anim I
snapshotEx = snapshot 2 circleTriangle

------------------
movingCircleRight :: Anim F
movingCircleRight = movingCircle' <-> background

movingCircleUP :: Anim F
movingCircleUP = movingCircleV <-> background

arrowsEx :: Anim F
arrowsEx = (movingCircle'->> movingCircleV) <-> background

arrowsEx1 :: Anim F
arrowsEx1 =
  ( movingCircle'
    ->> movingCircle' # rotateBy (1/4) # translateX 1
  )

  <-> background

-- xTrans :: Active Double F (T2 Double)
-- xTrans = translationX <:$> interval 0 1

-- yTrans :: Active Double F (T2 Double)
-- yTrans = translationY <:$> interval 0 1

-- arrowsEx2 :: Anim F
-- arrowsEx2 = transform <:$> (xTrans ->- yTrans) <:*> ipure(circle 0.25 # fc blue) <:> background
-- ------------------
-- -- runActive
-- --runActiveEx :: Anim F
-- --runActiveEx = runActive circleCircle (2)

-- ------------------
-- -- movie
-- movieEx :: Anim F
-- movieEx = movie [arrowsEx, arrowsEx2, circleCircle] -- only works for two elements

------------------
-- stack
movingCircleY :: Anim F
movingCircleY =
  translateY
  <:$> (sin <:$> interval 0 (2*pi))
  <:*> ipure (circle 0.25 # fc yellow)

movingCircleYO :: Anim F
movingCircleYO =
  translateY
  <:$> ((\x -> - sin x) <:$> interval 0 (2*pi))
  <:*> ipure (circle 0.25 # fc purple)

movingCircle1 :: Anim F
movingCircle1 =
  translateX
  <:$> (sin <:$> interval 0 (2*pi))
  <:*> ipure (circle 0.25 # fc blue)

movingCircleO1 :: Anim F
movingCircleO1 =
  translateX
  <:$> ((\x -> - sin x) <:$> interval 0 (2*pi))
  <:*> ipure (circle 0.25 # fc green)

collision :: Anim F
collision =
  stack [movingCircleY, movingCircleYO, movingCircle1, movingCircleO1]
  <-> background

combine :: Anim F
combine = circleCircle2 <-> collision

example3 :: Anim F
example3 = circleCircle <-> collision

-- ------------------
-- -- a1 <> a2
example2 :: Anim F
example2 = circleCircle <> circleTriangle <> collision

------------------
-- overlapping circle
circleCircle2 :: Anim F
circleCircle2 =
  translate
  <:$> ((r2 . (cos &&& sin)) <:$> interval 0 (4*pi))
  <:*> ipure (circle 0.25 # fc purple)

circleCircle2M :: Anim F
circleCircle2M =
  translate
  <:$> ((r2 . (\x -> (2 * cos x, sin x))) <:$> interval 0 (4*pi))
  <:*> ipure (circle 0.15 # fc green)

circleCircleSun :: Anim F
circleCircleSun =
 (translateX <:$> ((\x -> - cos x/2) <:$> interval 0 (4*pi)) <:*>
 (translateY <:$> ((\x -> - sin x/2) <:$> interval 0 (4*pi)) <:*> (ipure (circle 0.5 # fc orange))))

-- rotating :: Anim F
-- rotating = (<>) <:$> stack [circleCircle2, circleCircleSun, circleCircle2M] <:*> background

-- ----------------
-- -- overlapping rectangles
-- blackRect :: Anim F
-- blackRect = (\t -> rect 2 2 # fc black # translateX t) <:$> interval 0 2

-- movingRect :: Anim F
-- movingRect = (<>) <:$> blackRect <:*> ipure (rect 4 2 # translateX 1 # fc white)

-- ------------------
-- -- dur
-- durEx :: Anim I
-- durEx =    (translateX <:$> ((\x -> cos (x/2)) <:$> dur) <:*> (translateY <:$>
--   ((\x -> cos (x/2))<:$> dur) <:*> (ipure (circle 0.25 # fc black))) <:> background)

-- ------------------
-- -- cut
-- cutEx :: Anim F
-- cutEx = cut (3) solarSystem


-- ------------------
-- -- solar system
-- earth :: Anim I
-- earth = ipure(circle 0.5 # fc blue)

-- moon :: Anim I
-- moon =    (translateX <:$> ((\x -> 0.7 * cos (6.2*x)) <:$> dur) <:*> (translateY <:$>
--   ((\x -> 0.7 * sin (3*x/2))<:$> dur) <:*> (ipure (circle 0.1 # fc green # lc red))))

-- moon2 :: Anim I
-- moon2 =    (translateX <:$> ((\x -> 0.7 * cos (5.2*x)) <:$> dur) <:*> (translateY <:$>
--   ((\x -> 0.7 * sin (2*x))<:$> dur) <:*> (ipure (circle 0.1 # fc green # lc yellow))))

-- mercury :: Anim I
-- mercury = ipure(circle 0.25 # fc pink)

-- venus :: Anim I
-- venus = ipure(circle 0.45 # fc purple)

-- mars :: Anim I
-- mars = ipure(circle 0.35 # fc deeppink)

jupiter :: Anim I
jupiter = ipure (circle 1 # fc peachpuff)

-- saturn :: Anim I
-- saturn = ipure(circle 0.85 # fc navajowhite)

-- uranus :: Anim I
-- uranus = ipure(circle 0.7 # fc deepskyblue)

-- neptune :: Anim I
-- neptune = ipure(circle 0.65 # fc dodgerblue)

-- pluto :: Anim I
-- pluto = ipure(circle 0.15 # fc yellow # lc pink)

-- sun :: Anim I
-- sun =
--  (translateX <:$> ((\x -> - cos x/4) <:$> dur) <:*>
--  (translateY <:$> ((\x -> - sin x/4) <:$> dur) <:*> (ipure (circle 1.5 # fc orange))))

-- sun2 :: Anim I
-- sun2 = (translateX (- 0.5) <:$> ipure(circle 1.4 # fc orange))

-- earthwM :: Anim I
-- earthwM = (<>) <:$> earth <:*> moon

-- marswM :: Anim I
-- marswM =(<>) <:$> mars <:*> moon2

-- solarSystem2 :: Anim I
-- solarSystem2 = (orbit 2.15  2.15  8.0  mercury) <:>
--                (orbit 3.70  2.65  6.7    venus) <:>
--                (orbit 5.80  3.50  6.0  earthwM) <:>
--                (orbit 8.00  4.45  5.0   marswM) <:>
--                (orbit 12.2  5.75  4.0  jupiter) <:>
--                (orbit 16.2  7.35  3.0   saturn) <:>
--                (orbit 20.5  9.20  2.0   uranus) <:>
--                (orbit 24.0  10.7  1.0  neptune) <:>
--                (orbit 27.0  12.5  0.8    pluto) <:> sun2 -- <:> background

-- solarSystem :: Anim I
-- solarSystem = (translateX <:$> ((\x -> (2.15) * cos (8*x)) <:$> dur) <:*>
--          (translateY <:$> ((\x -> (2.15) * sin (8*x)) <:$> dur) <:*>
--          (mercury)) ) <:>

--          (translateX <:$> ((\x -> (3.70) * cos (6.7*x)) <:$> dur) <:*>
--          (translateY <:$> ((\x -> (2.65) * sin (6.7*x)) <:$> dur) <:*>
--          (venus))  ) <:>

--          (translateX <:$> ((\x -> (5.80) * cos (6*x)) <:$> dur) <:*>
--          (translateY <:$> ((\x -> (3.50) * sin (6*x)) <:$> dur) <:*>
--          (earthwM) ) <:>

--          (translateX <:$> ((\x -> (8.00) * cos (5*x)) <:$> dur) <:*>
--          (translateY <:$> ((\x -> (4.45) * sin (5*x)) <:$> dur) <:*>
--          (marswM)  ) <:>

--          (translateX <:$> ((\x -> (12.2) * cos (4*x)) <:$> dur) <:*>
--          (translateY <:$> ((\x -> (5.75) * sin (4*x)) <:$> dur) <:*>
--          (jupiter)) ) <:>

--          (translateX <:$> ((\x -> (16.2) * cos (3*x)) <:$> dur) <:*>
--          (translateY <:$> ((\x -> (7.35) * sin (3*x)) <:$> dur) <:*>
--          (saturn))  ) <:>

--          (translateX <:$> ((\x -> (20.5) * cos (2*x)) <:$> dur) <:*>
--          (translateY <:$> ((\x -> (9.20) * sin (2*x)) <:$> dur) <:*>
--          (uranus))  ) <:>

--          (translateX <:$> ((\x -> (24.0) * cos (1*x)) <:$> dur) <:*>
--          (translateY <:$> ((\x -> (10.7) * sin (1*x)) <:$> dur) <:*>
--          (neptune)) ) <:>

--          (translateX <:$> ((\x -> (27.0) * cos (0.8*x)) <:$> dur) <:*>
--          (translateY <:$> ((\x -> (12.5) * sin (0.8*x)) <:$> dur) <:*>
--          (pluto))  ) <:> sun2 <:> background) )

------------------
main :: IO ()
main = do
  let frames = simulate 25 (collision <-> (scale (1/3) <:$> background))
  forM_ (zip [0 :: Int ..] frames) $ \(i,frame) -> do
    renderRasterific (printf "out/frame%03d.png" i) (mkWidth 400) frame
  -- pic <- loadImageEmb "stars.jpg"
  -- case pic of
  --   Left st -> putStrLn st
  --   Right img -> do
  --       let background2 ::  Diagram B
  --           background2 =  image img # sized (mkWidth 55)
  --           frames = simulate 25 (cut 20  solarSystem2 <:> ipure(background2))
  --       forM_ (zip [0 :: Int ..] frames) $ \(i,frame) -> do
  --         renderRasterific (printf "out/frame%03d.png" i) (mkWidth 400) frame

-- {-

-- ghc --make Test && ./Test && cd out && ffmpeg -i frame%03d.png -vcodec mpeg4 test.mov && open test.mov && cd ..

-- ghc --make Test && rm -f out/*.png &&  ./Test && cd out && ffmpeg -i frame%03d.png -vcodec mpeg4 test.mov && open test.mov && cd ..



-- -}
