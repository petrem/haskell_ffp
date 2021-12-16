--{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where



functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y =
    if x > y then True else False

functionS :: (a, b) -> b
functionS (_, y) = y

r :: [a] -> [a]
r [] = []
r (x:xs) = r xs ++ [x]

co :: (b -> c) -> (a -> b) -> a -> c
co f g = f . g

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' f = f
