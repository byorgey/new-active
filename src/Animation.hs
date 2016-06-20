{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

import           Diagrams.Core
import           Diagrams.TrailLike

import           Control.Applicative  (liftA2)
import           Control.IApplicative hiding ((<*>))
import           Data.Active
import           Data.Finitude
import           Data.Monoid

type QAnimation b v n m = Active n F (QDiagram b v n m)
type Animation b v n = QAnimation b v n Any

type instance V (Active n f a) = V a
type instance Diagrams.Core.N (Active n f a) = Diagrams.Core.N a

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
