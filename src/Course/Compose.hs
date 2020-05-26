{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) f (Compose a) = Compose ((f <$>) <$> a)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure =
    Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
  -- (<*>) (Compose f) (Compose a) = Compose ((_f f) a)
  -- _f :: f (g (a -> b)) -> f (g a) -> f (g b)
  -- (<*>) (Compose f) (Compose a) = Compose ((pure (<*>) <*> f) <*> a)
  -- OR
  -- (<*>) (Compose f) (Compose a) = Compose ((<*>) (_f f) a)
  -- _f :: (f (g (a -> b)) -> f (g a -> g b)
  -- Look at the inner g (a -> b) -> g a -> g b
  -- To make <*> work in the inner Applicative, 
  -- (<*>) (Compose f) (Compose a) = Compose ((<*>) (pure (<*>) <*> f) a)
  -- (<*>) (Compose f) (Compose a) = Compose ((pure (<*>)) <*> f <*> a)
  (<*>) (Compose f) (Compose a) = Compose (lift2 (<*>) f a)

  -- f (g (a -> b)) -> f (g a) -> f (g b)
  -- (<*>) ::
  --   f (a -> b)
  --   -> f a
  --   -> f b
-- lift2 ::
--   Applicative f =>
--   (a -> b -> c)
--   -> f a
--   -> f b
--   -> f c
instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "impossible"
