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
    describe "Identity" $ do
      testBatch (traversable (undefined :: Identity HappyThreeFriends))
    describe "Constant" $ do
      testBatch (traversable (undefined :: Constant HappyThreeFriends HappyThreeFriends))
    describe "Opcional" $ do
      testBatch (traversable (undefined :: Opcional HappyThreeFriends))
    describe "List" $ do
      testBatch (traversable (undefined :: List HappyThreeFriends))
    describe "Three" $ do
      testBatch (traversable (undefined :: Three Int Int HappyThreeFriends))
    describe "Pair" $ do
      testBatch (traversable (undefined :: Pair Int HappyThreeFriends))
    describe "Big" $ do
      testBatch (traversable (undefined :: Big Int HappyThreeFriends))
    describe "Bigger" $ do
      testBatch (traversable (undefined :: Bigger Int HappyThreeFriends))
    describe "S" $ do
      testBatch (foldable (undefined :: S Maybe HappyFiveFriends))
      testBatch (traversable (undefined :: S Maybe HappyThreeFriends))
    describe "Tree" $ do
      testBatch (foldable (undefined :: Tree HappyFiveFriends))
      testBatch (traversable (undefined :: Tree HappyThreeFriends))

type HappyThreeFriends = (Int, String, [Int])
type HappyFiveFriends = (Int, String, [Int], Int, Char)
