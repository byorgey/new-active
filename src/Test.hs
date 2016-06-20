{-# LANGUAGE DataKinds #-}

import           Text.Printf

import qualified Animation                   as A
import           Control.IApplicative
import           Control.Monad               (forM_)
import           Data.Active
import           Data.Finitude

import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude            hiding (interval, simulate, ui)


movingCircle :: A.Animation Double F Rasterific V2 Double
movingCircle = (\t -> circle 1 # fc blue # translateX t) <:$> interval 0 2

movingCircle2 :: A.Animation Double F Rasterific V2 Double
movingCircle2 = (<>) <:$> movingCircle <:*> ipure (rect 4 2 # translateX 1 # fc white)

sinCircle :: A.Animation Double F Rasterific V2 Double
sinCircle =
  (<>) <:$> (translateX <:$> (sin <:$> interval 0 (6*pi)) <:*> (ipure (circle 1 # fc blue)))
       <:*> ipure (rect 4 2 # fc white)

circleCircle :: A.Animation Double F Rasterific V2 Double
circleCircle =
  (<>) <:$> (translateX <:$> (sin <:$> interval 0 (2*pi)) <:*> (translateY <:$> (cos <:$> interval 0 (2*pi)) <:*> (ipure (circle 0.25 # fc red))))
  <:*> ipure (rect 4 4 # fc white)

main :: IO ()
main = do
  let frames = simulate 30 circleCircle
  forM_ (zip [0 :: Int ..] frames) $ \(i,frame) -> do
    renderRasterific (printf "out/frame%03d.png" i) (mkWidth 400) frame

{-

