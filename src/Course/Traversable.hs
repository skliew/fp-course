{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose

-- | All instances of the `Traversable` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

instance Traversable List where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> List a
    -> f (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> ExactlyOne a
    -> f (ExactlyOne b)
  traverse f (ExactlyOne a) = ExactlyOne <$> f a

instance Traversable Optional where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Optional a
    -> f (Optional b)
  traverse _ Empty = pure Empty
  traverse f (Full a) = Full <$> f a

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA ::
  (Applicative f, Traversable t) =>
  t (f a)
  -> f (t a)
sequenceA = traverse id

-- Note: Remember how sequenceA works

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
-- Implement the traverse function for a Traversable instance for Compose
  -- traverse f (Compose a) = _f (traverse _g a)
  -- _g :: g a -> f0 b0
  -- For f (g a), traverse :: (g a -> h (g b)) -> f (g a) -> h (f (g a))
  -- For the input function f, traverse :: (a -> f1 b) -> g a -> f1 (g b)
  -- It could work because it's polymorphic in the g a position
  -- traverse (a -> t b) (f (g a)):: g a -> t (g b)
  -- To traverse with with (traverse f)
  -- traverse f (Compose a) = Compose <$> (traverse (_f f) a)
  -- _f :: (a -> f1 b) -> g a -> f1 (g b)
  -- Using this, traverse (traverse f) a :: (g a -> f1 (g b)) -> f (g a) -> f1 (f (g b))
  -- traverse f (Compose a) = _f (traverse (traverse f) a)
  -- We want our result to be f1 (Compose f g b), use <$>:
  traverse f (Compose a) = Compose <$> traverse (traverse f) a

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a) deriving (Show, Eq)

instance (Functor f, Functor g) =>
  Functor (Product f g) where
-- Implement the (<$>) function for a Functor instance for Product
  (<$>) f (Product a b) = Product (f <$> a) (f <$> b)

instance (Traversable f, Traversable g) =>
  Traversable (Product f g) where
-- Implement the traverse function for a Traversable instance for Product
  -- traverse f (Product a b) = _f (traverse f a) (traverse f b)
  -- traverse f (Product a b) = lift2 Product (traverse f a) (traverse f b)
  traverse f (Product a b) = Product <$> traverse f a <*> traverse f b

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)
  | InR (g a) deriving (Show, Eq)

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where
-- Implement the (<$>) function for a Functor instance for Coproduct
  (<$>) f (InL a) = InL (f <$> a)
  (<$>) f (InR a) = InR (f <$> a)

instance (Traversable f, Traversable g) =>
  Traversable (Coproduct f g) where
-- Implement the traverse function for a Traversable instance for Coproduct
  traverse f (InL a) = InL <$> traverse f a
  traverse f (InR a) = InR <$> traverse f a
