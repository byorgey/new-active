{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Animation where

import           Diagrams.Core
import           Diagrams.TrailLike

import           Control.Applicative  (liftA2)
import           Control.IApplicative hiding ((<*>))
import           Data.Active
import           Data.Finitude
import           Data.Monoid

type QAnimation t f b v n m = Active t f (QDiagram b v n m)
type Animation t f b v n = QAnimation t f b v n Any

type instance V (Active t f a) = V a
type instance Diagrams.Core.N (Active t f a) = Diagrams.Core.N a

instance HasOrigin a => HasOrigin (Active n f a) where
  moveOriginTo = imap . moveOriginTo

instance Transformable a => Transformable (Active n f a) where
  transform = imap . transform

instance HasStyle a => HasStyle (Active n f a) where
  applyStyle = imap . applyStyle

instance (TrailLike t, Num n, Ord n) => TrailLike (Active n I t) where
  trailLike = ipure . trailLike

instance (Juxtaposable a, Num n, Ord n) => Juxtaposable (Active n f a) where
  juxtapose = iliftA2 . juxtapose
