{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Data.Finitude where

data Finitude = F | I

type family Isect (f1 :: Finitude) (f2 :: Finitude) :: Finitude where
  Isect f f = f
  Isect F f = F
  Isect f F = F
  Isect f g = I

  -- or  Isect I g = g  ?

type family Union (f1 :: Finitude) (f2 :: Finitude) :: Finitude where
  Union F f = f
  Union I f = I


