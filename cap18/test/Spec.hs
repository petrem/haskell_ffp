module Main where

import Test.Hspec
import Test.Hspec.Checkers
import Test.Hspec.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import BadMonadSpec
import Lib


main :: IO ()
main = do
  --badMonadTests
  hspec $ do
    describe "Nope" $ do
      testBatch (functor (NopeDotJpg :: Nope HappyThreeFriends))
      testBatch (applicative (NopeDotJpg :: Nope HappyThreeMonoids))
      testBatch (monad (NopeDotJpg :: Nope HappyThreeFriends))
    describe "BahEither" $ do
      testBatch (functor (undefined :: BahEither String HappyThreeFriends))
      testBatch (applicative (undefined :: BahEither String HappyThreeMonoids))
      testBatch (monad (undefined ::BahEither String HappyThreeFriends))
    describe "Identity" $ do
      testBatch (functor (undefined :: Identity HappyThreeFriends))
      testBatch (applicative (undefined :: Identity HappyThreeMonoids))
      testBatch (monad (undefined :: Identity HappyThreeFriends))
    describe "List" $ do
      testBatch (functor (undefined :: List HappyThreeFriends))
      testBatch (monoid (undefined :: List Int))
      testBatch (applicative (undefined :: List HappyThreeMonoids))
      testBatch (monad (undefined :: List HappyThreeFriends))
    describe "Function j" $ do
      it "case 1" $ do
        j [[1, 2], [], [3]] `shouldBe` [1,2,3]
      it "case 2" $ do
        j (Just (Just 1)) `shouldBe` Just 1
      it "case 3" $ do
        j (Just Nothing::Maybe (Maybe String)) `shouldBe` Nothing
      it "case 4" $ do
        j (Nothing::Maybe (Maybe String)) `shouldBe` Nothing


type HappyThreeFriends = (Int, String, Char)
type HappyThreeMonoids = ([Int], String, [Char])

happyThreeFriends :: HappyThreeFriends
happyThreeFriends = undefined

