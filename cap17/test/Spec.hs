module Main where

import Control.Applicative
import Data.Monoid

import Test.Hspec
import Test.Hspec.Checkers
import Test.Hspec.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Lib


main :: IO ()
main = do
  hspec $ do
    describe "Identity" $ do
      prop "Applicative Identity" (prop_ApplicativeIdentity :: Identity String -> Bool)
      prop
        "Applicative Composability"
        (checkApplicativeComposable :: Identity Int -> Bool)
      prop
        "Applicative Homomorphism"
        (checkApplicativeHomomorphism :: Identity Int -> Int -> Bool)
      prop
        "Applicative Interchangeability"
        (checkApplicativeInterchangeability :: Identity Int -> Int -> Bool)
      testBatch (applicative $ Identity ("a", "b", 3 :: Int))
    describe "Constant" $ do
      testBatch (applicative (undefined :: Constant [Int] (Int, String, [Int])))
    describe "List" $ do
      testBatch (functor $ Cons (1 :: Int, "a", 'c') Nil)
      testBatch (monoid $ Cons (1 :: Int))
      testBatch (applicative $ Cons (1 :: Int, "a", 'b') Nil)
    describe "ZipList'" $ do
      it
        "ZipList' [(+9), (*2), (+8)] <*> ZipList' [1..3] `shouldBe` ZipList' [10, 4, 11]" $ do
        ZipList' [(+ 9), (* 2), (+ 8)] <*>
          ZipList' [1 .. 3] `shouldBe` ZipList' [10, 4, 11]
      it "ZipList' [(+9), (*2), (+8)] <*> pure1 `shouldBe` ZipList' [10, 2, 9]" $ do
        ZipList' [(+ 9), (* 2), (+ 8)] <*> pure 1 `shouldBe` ZipList' [10, 2, 9]
      testBatch (monoid $ ZipList' [Sum (1 :: Int)])
      testBatch (applicative $ ZipList' [("a", 'a', 1 :: Int)])
    describe "Validation" $ do
      it "Success (+1) <*> Success 1 == Success 2" $ do
        (Success (+ 1) :: Validation [String] (Int -> Int)) <*> Success 1
          `shouldBe` Success 2
      it "Failure [\"StackOverflow\"] <*> Success 1 == Failure [\"StackOverflow\"]" $ do
        (Failure ["StackOverflow"] :: Validation [String] (Int -> Int)) <*> Success 1
          `shouldBe` Failure ["StackOverflow"]
      it "Success (+1) <*> Failure [\"StackOverflow\"] == Failure [\"StackOverflow\"]" $ do
        (Success (+1) ::  Validation [String] (Int -> Int)) <*> Failure ["StackOverflow"]
          `shouldBe` Failure ["StackOverflow"]
      it "Failure [\"MooglesChewedWires\"] <*> Failure [\"StackOverflow\"] == Failure [\"MooglesChewedWires\", \"StackOverflow\"]" $ do
        (Failure ["MooglesChewedWires"] ::  Validation [String] (Int -> Int)) <*> Failure ["StackOverflow"]
          `shouldBe` Failure ["MooglesChewedWires", "StackOverflow"]
      testBatch (applicative (undefined :: Validation String (Int, String, [Int])))
    describe "Pair" $ do
      testBatch (functor $ Pair happyThreeFriends happyThreeFriends)
      testBatch (applicative $ Pair happyThreeFriends happyThreeFriends)
    describe "Two" $ do
      testBatch (functor $ Two happyThreeMonoids happyThreeFriends)
      testBatch (applicative $ Two happyThreeMonoids happyThreeFriends)
    describe "Three" $ do
      testBatch (functor $ Three happyThreeMonoids happyThreeMonoids happyThreeFriends)
      testBatch (applicative $ Three happyThreeMonoids happyThreeMonoids happyThreeFriends)
    describe "Three'" $ do
      testBatch (functor $ Three' happyThreeMonoids happyThreeFriends happyThreeFriends)
      testBatch (applicative $ Three' happyThreeMonoids happyThreeFriends happyThreeFriends)
    describe "Four" $ do
      testBatch (functor $ Four happyThreeFriends happyThreeFriends happyThreeFriends happyThreeFriends)
      testBatch (applicative $ Four happyThreeMonoids happyThreeMonoids happyThreeMonoids happyThreeFriends)
    describe "Four'" $ do
      testBatch (functor $ Four' happyThreeFriends happyThreeFriends happyThreeFriends happyThreeFriends)
      testBatch (applicative $ Four' happyThreeMonoids happyThreeMonoids happyThreeMonoids happyThreeFriends)


happyThreeFriends :: (Int, String, Char)
happyThreeFriends = undefined

happyThreeMonoids :: ([Int], String, [Char])
happyThreeMonoids = undefined

prop_ApplicativeIdentity :: (Applicative f, Eq (f a)) => f a -> Bool
prop_ApplicativeIdentity v = (pure id <*> v) == v

prop_ApplicativeComposable :: (Applicative f, Eq (f c)) => f (b -> c) -> f (a -> b) -> f a -> Bool
prop_ApplicativeComposable u v w = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

checkApplicativeComposable :: (Applicative f, Eq (f a), Num a) => f a -> Bool
checkApplicativeComposable = prop_ApplicativeComposable (pure f) (pure g)
  where
    f = (+2)
    g = (*3)

--prop_ApplicativeHomomorphism :: Applicative f => (a -> b) -> a -> f a -> f b --Bool
--prop_ApplicativeHomomorphism f x = (pure f <*> pure x) == pure (f x)

checkApplicativeHomomorphism :: (Applicative f, Eq (f a), Num a) => f a -> a -> Bool
checkApplicativeHomomorphism u a = left u a == right u a
  where
    f :: (Num a) => a -> a
    f = (+2)
    left :: (Applicative f, Num a) => f a -> a -> f a
    left _ x = pure f <*> pure x
    right :: (Applicative f, Num a) => f a -> a -> f a
    right _ x = pure f <*> pure x

prop_ApplicativeInterchangeability :: (Applicative f, Eq (f b)) => f (a -> b) -> a -> Bool
prop_ApplicativeInterchangeability u y = (u <*> pure y) == (pure ($ y) <*> u)

checkApplicativeInterchangeability :: (Applicative f, Num a, Eq (f a)) => f a -> a -> Bool
checkApplicativeInterchangeability u = prop_ApplicativeInterchangeability (const (+2) <$> u)
