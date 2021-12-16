{-# OPTIONS_GHC -Wno-type-defaults #-}

module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

testAddition :: IO ()
testAddition =
  hspec $ do
    describe "Addition" $ do
      it "1 + 1 is greater than 1" $ do (1 + 1) > 1 `shouldBe` True
      it "2 + 2 is equal to 4" $ do 2 + 2 `shouldBe` 4
      it "x + 1 is always greater than x" $ do property $ \x -> (x :: Int) + 1 > x

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

testDivideBy :: IO ()
testDivideBy =
  hspec $ do
    describe "DivideBy" $ do
      it "15 divided by 3 is 5" $ do dividedBy 15 3 `shouldBe` (5, 0)
      it "22 divided by 5 is 4 remainder 2" $ do dividedBy 22 5 `shouldBe` (4, 2)

prodr :: (Eq a, Ord a, Num a) => a -> a -> a
prodr n
  | n < 0 = (0 -) . go (-n)
  | otherwise = go n
  where
    go 0 x = 0
    go 1 x = x
    go n x = x + go (n - 1) x

testProdR :: IO ()
testProdR =
  hspec $ do
    describe "prodr" $ do
      it "1 times 5 is 5" $ do prodr 1 5 `shouldBe` 5
      it "5 times 1 is 5" $ do prodr 5 1 `shouldBe` 5
      it "5 times 5 is 25" $ do prodr 5 5 `shouldBe` 25
      it "0 times 5 is 0" $ do prodr 0 5 `shouldBe` 0
      it "5 times 0 is 0" $ do prodr 5 0 `shouldBe` 0
      it "-5 times 5 is -25" $ do prodr (-5) 5 `shouldBe` (-25)
      it "5 times -5 is -25" $ do prodr 5 (-5) `shouldBe` (-25)

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- What QuickCheck does so
-- you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return (Just a))]

-- frequency :: [(Int, Gen a)] -> Gen a
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
