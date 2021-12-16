module Cap9 where

import Data.Bool (bool)


myFilter = filter (not .(`elem` ["a", "an", "the"])) . words

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zip'' :: [a] -> [b] -> [(a,b)]
zip'' = zipWith' (,)

-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
  bool (myAnd xs) False (not x)

-- direct recursion, using (&&)
myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) = x && myAnd xs

myAnd'' :: [Bool] -> Bool
myAnd'' = foldr (&&) True

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (x':xs) = x == x' || myElem x xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

myReverse'' :: [a] -> [a]
myReverse'' = foldr (\x acc -> acc ++ [x]) []

squish :: [[a]] -> [a]
squish [] = []
squish (xs:xss) = xs ++ squish xss

squish' :: [[a]] -> [a]
squish' = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f xs = squish $ map f xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' _ [] = []
squishMap' f (a:as) = f a ++ squishMap' f as

squishAgain :: [[a]] -> [a]
squishAgain = squishMap' id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Empty list"
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = let mxs = myMaximumBy f xs
                       in bool x mxs (f x mxs == LT)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "Empty list"
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) = let mxs = myMinimumBy f xs
                       in bool x mxs (f x mxs == GT)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
