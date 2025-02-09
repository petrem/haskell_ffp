{-# LANGUAGE InstanceSigs #-}
module Lib where

import Control.Monad


-- Identity
newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

instance Functor Identity where
  fmap = (Identity .) . (. runIdentity)

instance Applicative Identity where
  pure = Identity
  Identity f <*> u = f <$> u

instance Foldable Identity where
  foldMap :: Monoid m => (a -> m) -> Identity a -> m
  foldMap = (. runIdentity)

instance Traversable Identity where
  sequenceA :: Functor f => Identity (f a) -> f (Identity a)
  sequenceA = (Identity <$>) . runIdentity

  traverse :: Applicative f => (a -> f b) -> Identity a -> f (Identity b)
  traverse f = (Identity <$>) .  f . runIdentity

instance Monad Identity where
  return = pure
  Identity a >>= f = f a


-- Const
newtype Const a b = Const { getConst :: a} deriving (Eq, Show)

instance Functor (Const a) where
  fmap :: (b -> c) -> Const a b -> Const a c
  fmap _ (Const a) = Const a

instance Monoid a => Applicative (Const a) where
  pure :: b -> Const a b
  pure = const $ Const mempty

  (<*>) :: Const a (b -> c) -> Const a b -> Const a c
  Const a1 <*> Const a2 = Const $ a1 <> a2

instance Foldable (Const a) where
  foldMap :: Monoid m => (b -> m) -> Const a b -> m
  foldMap _ = mempty

instance Traversable (Const a) where
  sequenceA :: Applicative f => Const a (f b) -> f (Const a b)
  sequenceA (Const a) = pure (Const a)

  traverse :: Applicative f => (b -> f c) -> Const a b -> f (Const a c)
  traverse _ (Const a) = pure (Const a)

instance Monoid a => Monad (Const a) where
  return = pure
  (>>=) :: Const a b -> (b -> Const a c) -> Const a c
  Const a >>= _ = Const a


-- Tuple (named product of two types)
data Tuple a b = Tuple a b deriving (Eq, Show)

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a (f b)

instance Monoid a => Applicative (Tuple a) where
  pure = Tuple mempty
  Tuple a1 f <*> Tuple a2 b = Tuple (a1 <> a2) (f b)

instance Foldable (Tuple a) where
  foldMap f (Tuple _ b) = f b

instance Traversable (Tuple a) where
  sequenceA (Tuple a fb) = Tuple a <$> fb
  traverse f (Tuple a b) = Tuple a <$> f b

instance Monoid a => Monad (Tuple a) where
  return = pure
  (>>=) :: Tuple a b -> (b -> Tuple a c) -> Tuple a c
  Tuple a1 b >>= f = let (Tuple a2 c) = f b in Tuple (a1<>a2) c


-- Union (named sum of two types)
data Union a b = Lefty a | Righty b deriving (Eq, Show)

instance Functor (Union a) where
  fmap _ (Lefty a) = Lefty a
  fmap f (Righty b) = Righty (f b)

instance Applicative (Union a) where
  pure = Righty
  Lefty l <*> _ = Lefty l
  Righty _ <*> Lefty l = Lefty l
  Righty f <*> Righty r = Righty (f r)

instance Foldable (Union a) where
  foldMap :: Monoid m => (b -> m) -> Union a b -> m
  foldMap _ (Lefty _) = mempty
  foldMap f (Righty r) = f r

instance Traversable (Union a) where
  sequenceA :: Applicative f => Union a (f b) -> f (Union a b)
  sequenceA (Lefty l) = pure (Lefty l)
  sequenceA (Righty fr) = Righty <$> fr

instance Monad (Union a) where
  return = pure
  (>>=) :: Union a b -> (b -> Union a c) -> Union a c
  (Lefty l) >>= _ = Lefty l
  (Righty r) >>= f = f r


-- Wrapper
newtype Wrapper f a = Wrapper { getWrapper :: f a } deriving (Eq, Show)

instance Functor f => Functor (Wrapper f) where
  fmap f u = Wrapper $ f <$> getWrapper u

instance Applicative f => Applicative (Wrapper f) where
  pure = Wrapper . pure
  Wrapper ff <*> Wrapper fa =  Wrapper $ ff <*> fa

instance Foldable f => Foldable (Wrapper f) where
  foldMap :: Monoid m => (a -> m) -> Wrapper f a -> m
  foldMap f (Wrapper fa) = foldMap f fa

instance Traversable f => Traversable (Wrapper f) where
  sequenceA :: Applicative g => Wrapper f (g a) -> g (Wrapper f a)
  sequenceA (Wrapper fga) = Wrapper <$> sequenceA fga

instance Monad f => Monad (Wrapper f) where
  return = pure
  (>>=) :: Wrapper f a -> (a -> Wrapper f b) -> Wrapper f b
  -- Wrapper fa >>= f = Wrapper . join $ fmap getWrapper (f <$> fa)
  Wrapper fa >>= f = Wrapper $ (f <$> fa) >>= getWrapper

-- fa (>>=) f = join (fmap f fa)


-- Compose
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fgab) <*> (Compose fga) = Compose $ fmap (<*>) fgab <*> fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  sequenceA :: Applicative h => Compose f g (h a) -> h (Compose f g a)
  sequenceA (Compose fgha) = Compose <$> sequenceA (sequenceA <$> fgha)

instance (Monad f, Monad g) => Monad (Compose f g) where
  return = pure
  (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
  (>>=) = undefined  -- cannot be done



x = 1 + 2
