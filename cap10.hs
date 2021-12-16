module Cap10 where

import Data.Bool (bool)


-- wrong foldl:
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ z [] = z
myFoldl f z (a:as) = f (myFoldl f z as) a

-- correct foldl
myFoldl' :: (b -> a -> b) -> b -> [a] -> b
myFoldl' _ z [] = z
myFoldl' f z (a:as) = myFoldl f (f z a) as

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z [] = z
myFoldr f z (a:as) = f a (myFoldr f z as)

demoAssoc :: String
demoAssoc = concat ["foldr: ", foldr f "0" xs, "\n",
                    "foldl: ", foldl f "0" xs, "\n"
                   ]
  where f x y = concat ["(", x, "+", y, ")"]
        xs = map show [(1::Integer)..5]

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsFirst10 :: [Integer]
fibsFirst10 = take 10 fibs

fibsFirstLessThan100 :: [Integer]
fibsFirstLessThan100 = takeWhile (<100) fibs

factorial :: [Integer]
factorial = scanl (*) 1 [2..]


stops  = "pbtdkg"
vowels = "aeiou"

sws :: [(Char, Char, Char)]
sws = [(s1, w, s2) | s1 <- stops, w <- vowels, s2 <- stops]

sws' :: [(Char, Char, Char)]
sws' = (,,) <$> stops <*> vowels <*> stops

triplets :: [a] -> [b] -> [(a, b, a)]
triplets as bs = (,,) <$> as <*> bs <*> as

sws'' = triplets stops vowels

nouns = ["rock", "scissors", "paper", "lizard", "Spock"]
verbs = ["crushes", "cuts", "covers", "poisons", "disproves"]

nonsense = triplets nouns verbs

seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x))) (length (words x))

seekritFunc' :: String -> Int
seekritFunc' = div <$> sum . map length . words <*> length . words

preciseSeekrit :: Fractional a => String -> a
preciseSeekrit = (/) <$> fromIntegral . sum . map length . words <*> fromIntegral . length . words

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' = flip foldr False . ((||) .)

myElemFold :: Eq a => a -> [a] -> Bool
myElemFold e = foldr ((||) .(== e)) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny e = any (== e)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

myReverse'' :: [a] -> [a]
myReverse'' = foldr (\a -> (++ [a])) []

myMap :: (a -> b) -> [a] -> [b]
myMap = flip foldr [] . ((:) .)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> bool acc (x:acc) (f x)) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = flip foldr [] . ((++) .)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldr1 (\a b -> bool b a (f a b == GT))

