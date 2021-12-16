{-# LANGUAGE NoImplicitPrelude #-}
module PrettyReader where

flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

const :: a -> b -> a
const a _ = a

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \a -> f (g a)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor ((->) r) where
  fmap = (.)

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative ((->) r) where
  pure = const
  -- f :: (->) r (a -> b)
  -- a :: (->) r a
  f <*> a = \r -> f r (a r)

class Applicative f => Monad f where
  return :: a -> f a
  (>>=) :: f a -> (a -> f b) -> f b

-- Note in general we can't simply get Monad from Applicative
-- but this is a specific instance
instance Monad ((->) r) where
  return = pure
  -- m :: (->) r a
  -- k :: a -> (->) r b
  -- k :: a -> r -> b
  -- flip k :: r -> a -> b
  -- flip k :: (->) r -> (a -> b)
  m >>= k = flip k <*> m
