module Cap7 where

addOneIfOdd n = case odd n of True -> f n
                              False -> n
  where f = \n ->  n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y =  f y x


k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)


f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))


functionC x y = case x > y of
                  True -> x
                  False -> y


ifEvenAdd2 n = case even n of
                 True -> n+2
                 False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2



tensDigit :: Integral a => a -> a
tensDigit = snd . (`divMod` 10) . fst . (`divMod` 10)

hundredsDigit :: Integral a => a -> a
hundredsDigit = snd . (`divMod` 10) . fst . (`divMod` 100)

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

foldBool :: a -> a -> Bool -> a
foldBool x y b | b         = y
               | otherwise = x

foldBool' :: a -> a -> Bool -> a
foldBool' x y b = case b of True -> y
                            _ -> x

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
