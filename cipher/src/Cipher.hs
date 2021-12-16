module Cipher where

import Data.Bool (bool)
import Data.Char


type IntKey = Int
type Key = String

type ClearText = String
type Cipher = String

caesar :: IntKey -> ClearText -> Cipher
caesar shift = map (shiftAlpha (+) shift)

uncaesar :: IntKey -> Cipher -> ClearText
uncaesar shift = map (shiftAlpha (-) shift)

vigenère :: Key -> ClearText -> Cipher
vigenère k = zipWith (shiftAlpha (+)) (map (subtract <$> baseCharCode <*> ord ) . concat . repeat $ k)

unvigenère :: Key -> Cipher -> ClearText
unvigenère k = zipWith (shiftAlpha (-)) (map (subtract <$> baseCharCode <*> ord ) . concat . repeat $ k)

baseCharCode :: Char -> Int
baseCharCode = ord . bool 'A' 'a' . isLower

shiftAlpha :: (Int -> Int -> Int) -> Int -> Char -> Char
shiftAlpha op shift c
  | (&&) <$> isAscii <*> isAlpha $ c =
      chr . (+ baseCharCode c) . (`mod` 26) . (`op` shift) . subtract (baseCharCode c) . ord $ c
  | otherwise =
      '?'
