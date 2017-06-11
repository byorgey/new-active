{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import           Control.Arrow ((&&&))
import           Text.Printf

import qualified Animation                   as A
import           Control.IApplicative
import           Control.Monad               (forM_)
import           Data.Active
import           Data.Finitude

import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude            hiding (interval, simulate, ui, (->>),
                                             backwards, stretchTo, stretch,
                                             runActive, snapshot, movie, imap, Active, (<->), toDuration)

type Anim f = A.Animation Double f Rasterific V2 Double

-- Stuff to provide in a diagrams-specific animation module:
--   fadeIn/fadeOut

theAnim :: Anim F
theAnim = movie
  [ triangleA # fadeIn 1
  , circle 1 # fc red # lasting 1
    <+> triangleA     # lasting 2
  , triangleA # fadeOut 1
  ]
  where
    triangleA :: Diagram B
    triangleA = fromOffsets [unitX, unitY] # closeTrail # stroke
    fadeIn d a  = opacity <:$> ((/d) <:$> interval 0 d) <:*> ipure a
    fadeOut d a = active (toDuration d) (\t -> a # opacity (1 - t/d))

main :: IO ()
main = do
  let frames = simulate 25 (theAnim <-> always (square 5 # fc white # lw 0))
  forM_ (zip [0 :: Int ..] frames) $ \(i,frame) -> do
    renderRasterific (printf "out/frame%03d.png" i) (mkWidth 400) frame

