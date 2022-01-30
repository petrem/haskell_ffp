module Lib where

import Data.List (isPrefixOf)
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = char '1' >> stop

-- read two characters, '1' and '2'
oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

oneEof :: Parser Char
oneEof = char '1' <* eof

oneTwoEof :: Parser Char
oneTwoEof = oneTwo <* eof

newtype Tolerant a = Tolerant { getParser :: Parser a}

instance Functor Tolerant where
  fmap f (Tolerant r) = Tolerant $ fmap f r

instance Applicative Tolerant where
  pure = Tolerant . pure
  Tolerant p1 <*> Tolerant p2 = Tolerant (p1 <*> p2)

instance Monad Tolerant where
  return = pure
  -- f::a -> Tolerant b 
  Tolerant p >>= f = undefined -- but what's the use...

preferSuccess :: Result a -> Result a -> Result a
preferSuccess (Failure _) r = r
preferSuccess r _ = r

tryGetFirstSuccess :: [Result a] -> Result a
tryGetFirstSuccess = foldr1 preferSuccess

--(string "a" >>= \r1 -> (string "b" >>= \r2 -> (eof >> return (r1++r2))))

-- I have no idea if this is what the exercise intended. It works like in the examples...
p123 :: String -> Result String
p123 input =
  let ps p = parseString p mempty input
  in tryGetFirstSuccess $ map (ps . ((<* eof) . string)) ["abc", "ab", "a"]
    
tryParse :: (a -> Parser a) -> [a] -> Parser a
tryParse p xs  = undefined

-- | a 'string' made of 'char's
charString :: String -> Parser String
charString = traverse char


testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

mainly :: IO ()
mainly = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneEof:"
  testParse oneEof
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'


yourFuncHere :: Parser Integer
yourFuncHere = integer <* eof
