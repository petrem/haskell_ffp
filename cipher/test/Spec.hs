import Test.Hspec
import Test.Hspec.QuickCheck -- for prop
import Test.QuickCheck

import Cipher


main :: IO ()
main = hspec $ do
  describe "caesar" $ do
    prop "encoding and decoding returns original message" prop_inverse_caesar
  describe "vigenere" $ do
    prop "encoding and decoding returns original message" prop_inverse_vigenere

newtype AlphaString = AlphaString String deriving (Eq, Show)

alphaCharSet :: String
alphaCharSet = ['A'..'Z'] ++ ['a'..'z']

genAlphaString :: Int -> Gen AlphaString
genAlphaString size = do
  AlphaString <$> sequenceA [elements alphaCharSet | _ <- [0..size]]

instance Arbitrary AlphaString where
  arbitrary = sized genAlphaString

prop_inverse_caesar :: NonNegative IntKey -> AlphaString -> Bool
prop_inverse_caesar (NonNegative k) (AlphaString m) = (uncaesar k . caesar k $ m) == m

prop_inverse_vigenere :: AlphaString -> AlphaString -> Bool
prop_inverse_vigenere (AlphaString k) (AlphaString m) = (unvigenère k . vigenère k $ m) == m

