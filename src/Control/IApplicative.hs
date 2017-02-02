{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Control.IApplicative where

-- | Monoidally indexed functors.  @imap@ preserves the index.
class IFunctor (f :: k -> * -> *) where
  imap :: (a -> b) -> f i a -> f i b

-- | A convenient synonym for 'imap'.
(<:$>) :: IFunctor f => (a -> b) -> f i a -> f i b
(<:$>) = imap

-- | Monoidally indexed applicative functors.
class IFunctor f => IApplicative (f :: k -> * -> *) where
  -- | The identity element of a type-level monoid.
  type Id :: k
  -- | The associative combining operation of a type-level monoid.
  type (:*:) (a :: k) (b :: k) :: k

  -- | 'ipure' creates a value with an identity type
  --   index.
  ipure :: a -> f Id a

  -- | '(<:*>)' combines type indices.
  (<:*>) :: f i (a -> b) -> f j a -> f (i :*: j) b

iliftA2 :: IApplicative f => (a -> b -> c) -> f i a -> f j b -> f (i :*: j) c
iliftA2 g x y = g <:$> x <:*> y

------------------------------------------------------------
-- Turning normal functors into indexed ones

-- | Turn a normal functor into an indexed one.
newtype Ixed f (i :: *) (a :: *) = Ixed (f a)
  deriving Show

instance Functor f => IFunctor (Ixed f) where
  imap f (Ixed x) = Ixed (fmap f x)

-- | A normal applicative functor can be treated as an indexed
--   applicative functor with a trivial index.
instance Applicative f => IApplicative (Ixed f) where
  type Id = ()
  type (:*:) i j = ()
  ipure = Ixed . pure
  Ixed f <:*> Ixed x = Ixed (f <*> x)

