import Test.Hspec
import Test.Hspec.QuickCheck -- for prop
import Test.QuickCheck

import Split


main :: IO ()
main = hspec $ do
  describe "split" $ do
    prop "unsplit is left inverse of split" prop_split_inv

-- show
prop_split_inv :: Char -> String -> Bool
prop_split_inv c xs = unsplit c (split c xs) == xs
