module WordNumber ( digitToWord
                  , digits
                  , wordNumber
                  ) where

import Data.List (intersperse)


digitToWord :: Int -> String
digitToWord n = case n of 0 -> "zero"
                          1 -> "one"
                          2 -> "two"
                          3 -> "three"
                          4 -> "four"
                          5 -> "five"
                          6 -> "six"
                          7 -> "seven"
                          8 -> "eight"
                          9 -> "nine"
                          _ -> error "This is no digit, fool"

digits :: Int -> [Int]
digits n = go n []
  where
    go m xs | m < 10 = m : xs
            | otherwise = let (q, r) = divMod m 10
                          in go q xs ++ r:xs

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
