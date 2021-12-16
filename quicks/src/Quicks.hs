module Quicks where

import Data.Char (toUpper)
import Data.List (sort)
import Test.QuickCheck


data Trivial =
  Trivial
  deriving (Eq, Show)


trivialGen :: Gen Trivial
trivialGen =
  return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen


data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen


data Pair a b =
  Pair a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)


data Sum a b = First a
             | Second b
             deriving (Eq, Show)

-- equal odds for each
sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [ return $ First a
        , return $ Second b
        ]

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a),
             (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = sumGenEqual

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

half :: Floating a => a -> a
half x = x / 2

square :: Num a => a -> a
square x = x * x


capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord xs = (:) <$> toUpper . head <*> tail $ xs


twice f = f . f

fourTimes = twice . twice

idempotence1 x =
  (capitalizeWord x == twice capitalizeWord x) &&
  (capitalizeWord x == fourTimes capitalizeWord x)

idempotence2 x = (sort x == twice sort x) && (sort x == fourTimes sort x)



data Fool =
  Fulse
  | Frue
  deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary = oneof [return Fulse, return Frue]

foolGen :: Gen Fool
foolGen = arbitrary

newtype BiasedFool = BiasedFool Fool deriving (Eq, Show)

instance Arbitrary BiasedFool where
  arbitrary = BiasedFool <$> frequency[(2, return Fulse), (1, return Frue)]

biasedFoolGen :: Gen BiasedFool
biasedFoolGen = arbitrary
