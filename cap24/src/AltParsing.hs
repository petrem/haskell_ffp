{-# LANGUAGE QuasiQuotes #-}
module AltParsing where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ

type NumberOrString =
  Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser [NumberOrString]
parseNos =
  (skipMany $ char '\n') *>
  (some $ (Left <$> integer) <|> (Right <$> some letter))
  <* (many $ char '\n')

parseNos1 :: Parser NumberOrString
parseNos1 =
  (skipMany $ oneOf "\r\n \t") *>
  (Left <$> integer) <|> (Right <$> some letter)
  <* (skipMany $ oneOf "\r\n \t")

data NumberFooString = A Integer | B String deriving (Show)

parseNos' :: Parser NumberFooString
parseNos' =
  (A <$> integer) <|> (B <$> some letter)

altMain = do
  let p f i = parseString f mempty i
  print $ p (some letter) a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c
  print $ p (some parseNos) c
  print $ p (some parseNos') c
  print $ p (some parseNos1) eitherOr

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

data MyName = MyName String deriving Show
