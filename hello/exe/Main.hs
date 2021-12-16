module Main where

import System.IO

import DogsRule
import Hello


main :: IO ()
main = do
  putStrLn "And what's your name, puny human?"
  hSetBuffering stdout NoBuffering
  putStr "> "
  name <- getLine
  sayHello name
  dogs
