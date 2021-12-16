module Palindrome where

import Control.Monad
import Data.Char
import System.Exit (exitFailure)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case isPalindrome line1 of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitFailure

isPalindrome :: String -> Bool
isPalindrome xs = ls == reverse ls
  where ls = map toLower . filter isAlpha $ xs
