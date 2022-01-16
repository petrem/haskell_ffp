module FizzBuzz where

import Control.Monad.Trans.State
import qualified Data.DList as DL


fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz" 
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

printFB :: IO ()
printFB = mapM_ (putStrLn . fizzBuzz) [1..100]

-- State fizzBuzz, take 1

addSolution :: Integer -> State [String] ()
-- addSolution n = do
--   xs <- get
--   let x = fizzBuzz n
--   put (x:xs)
addSolution n =
  get >>= \xs -> put (fizzBuzz n:xs)

-- get :: State s s ??
-- put :: s -> State s ()

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList xs = execState (mapM_ addSolution xs) []

printStateFB :: IO ()
printStateFB = mapM_ putStrLn (reverse $  fizzBuzzList [1..100])


-- State fizzBuzz, using DList

fizzBuzzDList :: [Integer] -> DL.DList String
fizzBuzzDList xs = execState (mapM_ dlistAddSolution xs) DL.empty

dlistAddSolution :: Integer -> State (DL.DList String) ()
dlistAddSolution n = do
  xs <- get
  let x = fizzBuzz n
  put (DL.snoc xs x)

printDListFB :: IO ()
printDListFB = mapM_ putStrLn (fizzBuzzDList [1..100])

-- State fizzBuzz, enumerating


fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = execState (mapM_ addSolution (enumFromThenTo from then_ to)) []
  where then_ = if from <= to then succ from else pred from

printEnumeratedFizzBuzz :: IO ()
printEnumeratedFizzBuzz = mapM_ putStrLn (fizzbuzzFromTo 100 1)
