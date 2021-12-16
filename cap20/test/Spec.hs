import Data.Semigroup
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Checkers
import Test.Hspec.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Lib


main :: IO ()
main = do
  hspec $ do
    describe "product'" $ do
      it "should be 1 for empty structure" $ do
        (product' Nothing :: Int) `shouldBe` (1::Int)
    describe "Constant" $ do
      testBatch (foldable (undefined :: Constant Int HappyFiveFriends))
    describe "Two" $ do
      testBatch (foldable (undefined :: Two Int HappyFiveFriends))
    describe "Three" $ do
      testBatch (foldable (undefined :: Three Int Int HappyFiveFriends))
    describe "Three'" $ do
      testBatch (foldable (undefined :: Three' Int HappyFiveFriends))
    describe "Four'" $ do
      testBatch (foldable (undefined :: Four' Int HappyFiveFriends))
    describe "filterF" $ do
      it "Sum even numbers" $ do
        (filterF even [1,2,3,4,5,6] :: Sum Int) `shouldBe` Sum 12
      it "Anonymous product to Maybe, when true" $ do
        (filterF (const True) ("foldable", "me") :: Maybe String) `shouldBe` Just "me"
      it "Anonymous product to Maybe, when false" $ do
        (filterF (const False) ("foldable", "me") :: Maybe String) `shouldBe` Nothing
    describe "filterF'" $ do
      it "Sum even numbers" $ do
        (filterF' even [1,2,3,4,5,6] :: Sum Int) `shouldBe` Sum 12
      it "Anonymous product to Maybe, when true" $ do
        (filterF' (const True) ("foldable", "me") :: Maybe String) `shouldBe` Just "me"
      it "Anonymous product to Maybe, when false" $ do
        (filterF' (const False) ("foldable", "me") :: Maybe String) `shouldBe` Nothing

type HappyFiveFriends = (Int, String, [Int], Int, Char)
