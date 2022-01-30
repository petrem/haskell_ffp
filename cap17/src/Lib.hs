module Lib where

-- Cap 17 - Applicative

import Control.Applicative
import Data.Monoid

import Test.QuickCheck
  ( Arbitrary
  , CoArbitrary
  , Gen
  , arbitrary
  , choose
  , coarbitrary
  , frequency
  , sized
  , variant
  )
import Test.QuickCheck.Checkers (EqProp, (=-=), eq)
import Test.QuickCheck.Classes


--

class StructuralEq f where
  (=*=) :: f a -> f b -> Bool
  -- should have similar laws as Eq:
  --   Reflexivity
  --     x == x = True
  -- Symmetry
  --     x == y = y == x
  -- Transitivity
  --     if x == y && y == z = True, then x == z = True
  -- Substitutivity
  --     if x == y = True and f is a "public" function whose return type is an instance of Eq, then f x == f y = True
  -- Negation
  --     x /= y = not (x == y)

--

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance StructuralEq Identity where
  (=*=) = const . const True

--

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a


instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant a1) <*> (Constant a2) = Constant (a1 <> a2)

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => StructuralEq (Constant a) where
  (Constant a1) =*= (Constant a2) = a1 == a2

--

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Foldable List where
  foldr _ b Nil = b
  foldr f b (Cons a l) = f a (foldr f b l)

instance Traversable List where
  sequenceA = foldr (\a b -> Cons <$> a <*> b) (pure Nil)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = arbitraryList
    where
      arbitraryList :: Arbitrary a => Gen (List a)
      arbitraryList = sized $ \n -> do
        k <- choose(0, n)
        let l = foldr Cons Nil [arbitrary | _ <- [1..k]]
        sequenceA l

instance CoArbitrary a => CoArbitrary (List a) where
  -- TODO: I have no idea why this works :-(
  -- See: https://carlo-hamalainen.net/2018/01/30/quickchecks-coarbitrary-generate-random-functions/
  coarbitrary Nil = variant (0::Integer)
  coarbitrary (Cons a l) = variant (1::Integer) . coarbitrary (a, l)

listTake :: Int -> List a -> List a
listTake _ Nil = Nil
listTake 0 _ = Nil
listTake n (Cons a l) = Cons a (listTake (n - 1) l)

instance Eq a => EqProp (List a) where
--  (=-=) = eq
  xs =-= ys = listTake 200 xs `eq` listTake 200 ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Semigroup (List a) where
  Nil <> l = l
  l <> Nil = l
  (Cons a at) <> l = Cons a (at <> l)

instance Monoid (List a) where
  mempty = Nil

instance Applicative List where
  pure = flip Cons Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f ft) <*> as = fmap f as <> (ft <*> as)

fromFoldable :: Foldable f => f a -> List a
fromFoldable = foldr Cons Nil

listConcat :: List (List a) -> List a
listConcat = foldr mappend Nil

listConcatMap :: (a -> List b) -> List a -> List b
listConcatMap = (listConcat .) . fmap

--

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take 3000 l
          ys' = let (ZipList' l) = ys in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . (: [])
  (ZipList' []) <*> _ = ZipList' []
  _ <*> (ZipList' []) = ZipList' []
  (ZipList' fs) <*> (ZipList' xs) =
    ZipList' $ zipWith ($) (extendTo (length xs) fs) (extendTo (length fs) xs)
    where
      extendTo n ls =
        let m = max 0 (n - length ls)
         in ls ++ replicate m (last ls)

instance Semigroup a => Semigroup (ZipList' a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

--

data Validation err a = Failure err
                      | Success a deriving (Eq, Show)

validationToEither :: Validation e a -> Either e a
validationToEither (Failure err) = Left err
validationToEither (Success a) = Right a

eitherToValidation :: Either e a -> Validation e a
eitherToValidation (Left err) = Failure err
eitherToValidation (Right a) = Success a

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    frequency [(4, return $ Failure e), (6, return $ Success a)]

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  Failure f <*> _ = Failure f
  Failure err1 <*> Failure err2 = Failure (err1 <> err2)
  Failure err <*> Success _ = Failure err
  Success _ <*> Failure err = Failure err
  Success f <*> Success a = Success (f a)

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

--

data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure = Pair <$> id <*> id
  Pair f g <*> Pair a a' = Pair (f a ) (g a')

--

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two a f) <*> (Two a' b) = Two (a <> a') (f b)

--

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') (f c)

--

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
  pure = Three' mempty <$> id <*> id
  (Three' a f g) <*> (Three' a' b b') = Three' (a <> a') (f b) (g b')

--

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four a b c f) <*> (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

--

data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' a1 a2 a3 f) <*> (Four' a1' a2' a3' b) = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f b)

--

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
