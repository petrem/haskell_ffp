import Test.Hspec
import Test.QuickCheck

import WordNumber (digitToWord, digits, wordNumber)


main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1,0,0]
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
    it "adds a dash for each new order of magnitude" $ do
      quickCheck prop_dashForEachOrderOfMagnitude


newtype Positive' a = Positive' a deriving (Eq, Show)

genPositive :: (Arbitrary a, Enum a, Ord a, Num a) => Gen (Positive' a)
genPositive = do
  a <- arbitrary
  elements $ map Positive' . take 100 . filter ( >= 0) $ [a..]

instance (Num a, Ord a, Enum a, Arbitrary a) => Arbitrary (Positive' a) where
  arbitrary = genPositive


prop_dashForEachOrderOfMagnitude :: Positive' Int -> Bool
prop_dashForEachOrderOfMagnitude (Positive' x) =
  nDigitsOverOne x == (length . filter (== '-') . wordNumber $ x)
  where
    nDigitsOverOne :: Int -> Int
    nDigitsOverOne n | n == 0 = 0
                     | otherwise = floor . logBase 10 . fromIntegral $ n
