import Test.Hspec
import Test.Hspec.QuickCheck

import Quicks


main :: IO ()
main = do
  hspec $ do
    describe "idempotence of f = idempotence1" $ do
      prop "is idempotent" prop_Idempotent1

    describe "idempotence of f' = idempotence2" $ do
      prop "is idempotent" prop_Idempotent2


prop_Idempotent1 :: String -> Bool
prop_Idempotent1 = idempotence1

prop_Idempotent2 :: [String] -> Bool
prop_Idempotent2 = idempotence2

