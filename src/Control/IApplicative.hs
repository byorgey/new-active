{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Control.IApplicative where

class IFunctor (f :: k -> * -> *) where
  imap :: (a -> b) -> f i a -> f i b

(<:$>) :: IFunctor f => (a -> b) -> f i a -> f i b
(<:$>) = imap

class IFunctor f => IApplicative (f :: k -> * -> *) where
  type Id :: k
  type (:*:) (a :: k) (b :: k) :: k
  ipure :: a -> f Id a
  (<:*>) :: f i (a -> b) -> f j a -> f (i :*: j) b

iliftA2 :: IApplicative f => (a -> b -> c) -> f i a -> f j b -> f (i :*: j) c
iliftA2 g x y = g <:$> x <:*> y

------------------------------------------------------------
-- Turning normal functors into indexed ones

newtype Ixed f (i :: *) (a :: *) = Ixed (f a)
  deriving Show

instance Functor f => IFunctor (Ixed f) where
  imap f (Ixed x) = Ixed (fmap f x)

instance Applicative f => IApplicative (Ixed f) where
  type Id = ()
  type (:*:) i j = ()
  ipure = Ixed . pure
  Ixed f <:*> Ixed x = Ixed (f <*> x)

