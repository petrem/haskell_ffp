module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key -> Nothing
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' = flip lookup (zip x z)

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $
    sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
{-
Just [3,2,1]
--[[1,2,3] X [4,5,6]]--
Just [6, 9]
Just 15
Nothing
True
[True, False, False]
-}
  print $ sequenceA [(>3), (<8), even] 7 -- [True, True, False]
  -- 1. Fold the Boolean conjunction operator over the list of results of sequA
  -- (applied to some value).
  putStrLn "--- 1 ---"
  print $ and . sequA $ 7
  print $ foldr1 (&&) . sequA $ 7
  -- 2. Apply sequA to s'—you’ll need fromMaybe
  putStrLn "--- 2 ---"
  print $ sequA $ fromMaybe 0 s'
  -- 3. Apply bolt to ys—you’ll need fromMaybe
  putStrLn "--- 3 ---"
  print $ bolt $ fromMaybe 0 ys
