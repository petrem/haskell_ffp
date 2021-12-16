module Main where

import qualified Data.Map as M

import Test.QuickCheck

import Morse


main :: IO ()
main = do
  quickCheck prop_thereAndBackAgain

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen
  (\c -> (charToMorse c >>= morseToChar) == Just c)

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse
