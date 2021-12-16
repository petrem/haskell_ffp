module Cap8 where

import Data.List (intersperse)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ = id
applyTimes n f = f . applyTimes (n-1) f

applyUntil :: (a -> b -> Bool) -> (a -> a) -> a -> (b -> b) -> b -> b
applyUntil check next a f b | check a b = b
                            | otherwise = f . applyUntil check next (next a) f $ b

applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' = applyUntil (\x _ -> x == 0) (subtract 1)


fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

sumr :: (Eq a, Num a) => a -> a
sumr 0 = 0
sumr n = n + sumr (n-1)

prodr :: (Integral a) => a -> a -> a
prodr 1 x = x
prodr n x = x + prodr (n-1) x


data DividedResult a = Result a
                     | DividedByZero deriving Show

divideBy :: (Integral a) => a -> a -> DividedResult a
divideBy _ 0  = DividedByZero
divideBy n d = go (abs n) (abs d) 0
  where
    resultSign = (signum n) * (signum d)
    go n' d' c | n' < d' = Result (resultSign * c)
               | otherwise = go (n' - d') d' (c+1)

mc91 :: Integral a => a -> a
mc91 n | n <= 100 = mc91 (mc91 $ n + 11)
     | otherwise = n - 10



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
