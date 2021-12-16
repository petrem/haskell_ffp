module Lib where

-- Cap 20 - Foldable

import Data.Bool (bool)
import Data.Semigroup
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers


sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum


product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = getAny . foldMap (Any . (== e))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr g Nothing
  where
    g a Nothing = Just a
    g a (Just b) = Just $ min a b

data MiniMe a = Bigestest | Only a deriving Eq


instance Ord a => Ord (MiniMe a) where
  compare Bigestest Bigestest = EQ
  compare Bigestest _ = GT
  compare _ Bigestest = LT
  compare (Only a) (Only b) = compare a b

fromMiniMeToMaybe :: MiniMe a -> Maybe a
fromMiniMeToMaybe Bigestest = Nothing
fromMiniMeToMaybe (Only a) = Just a

minimum'' :: (Foldable t, Ord a) => t a -> Maybe a
minimum'' = fromMiniMeToMaybe . foldr (min . Only) Bigestest

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (max . Just) Nothing

null' :: (Foldable t) => t a -> Bool
null' = getAll . foldMap (const $ All False)

length' :: (Foldable t) => t a -> Int
length' = foldr (const (+1)) 0

length'' :: (Foldable t) => t a -> Int
length'' = getSum . foldMap (Sum . const 1)

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- | Combine the elements
--   of a structure using a monoid.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMapFromFoldr :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMapFromFoldr f = foldr ((<>) . f) mempty

data Constant a b = Constant b deriving (Show, Eq)

instance Arbitrary b => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq b => EqProp (Constant a b) where
  (=-=) = eq

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b
  foldr f z (Constant b) = f b z

--

data Two a b = Two a b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b
  foldr f z (Two _ b) = f b z

--

data Three a b c = Three a b c deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c
  foldr f z (Three _ _ c) = f c z

--

data Three' a b = Three' a b b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Foldable (Three' a) where
  foldMap f (Three' _ b1 b2) = f b1 <> f b2
  foldr f z (Three' _ b1 b2) = f b1 (f b2 z)

--

data Four' a b = Four' a b b b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance Foldable (Four' a) where
  foldMap f (Four' _ b1 b2 b3) = f b1 <> f b2 <> f b3
  foldr f z (Four' _ b1 b2 b3) = f b1 (f b2 (f b3 z))

--

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap g
  where g a | f a = pure a
            | otherwise = mempty

filterF' :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF' f = foldMap ((bool mempty . pure) <*> f)
