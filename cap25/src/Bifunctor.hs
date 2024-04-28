module Bifunctor where

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id


data Deux a b = Deux a b deriving (Eq, Show)

instance Functor (Deux a) where
  fmap f (Deux a b) = Deux a (f b)

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)


data Const a b = Const a deriving (Eq, Show)

instance Functor (Const a) where
  fmap _ (Const a) = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)


data Drei a b c = Drei a b c deriving (Eq, Show)

instance Functor (Drei a b) where
  fmap f (Drei a b c) = Drei a b (f c)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)


data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)

instance Functor (SuperDrei a b) where
  fmap _ (SuperDrei a b) = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)


data SemiDrei a b c = SemiDrei a deriving (Eq, Show)

instance Functor (SemiDrei a b) where
  fmap _ (SemiDrei a) = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a


data Quadriceps a b c d = Quadzzz a b c d deriving (Eq, Show)

instance Functor (Quadriceps a b c) where
  fmap f (Quadzzz a b c d) = Quadzzz a b c (f d)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)


data Buridan a b = Water a | Hay b deriving (Eq, Show)

instance Functor (Buridan a) where
  fmap _ (Water a) = Water a
  fmap f (Hay b) = Hay (f b)

instance Bifunctor Buridan where
  bimap f _ (Water a) = Water (f a)
  bimap _ g (Hay b) = Hay (g b)
