{-# LANGUAGE OverloadedStrings #-}
module BT where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

-- helper for trifecta
trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

-- helper for parsec
parsecP :: (Show a) => Parsec String () a ->  String -> IO ()
parsecP = parseTest

-- helper for attoparsec
attoP :: Show a=> A.Parser a -> ByteString -> IO ()
attoP p i = print $ parseOnly p i


nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

backParse :: (Monad f, CharParsing f) => f Char
backParse = try ((char '1' >> char '2') <?> "try 1 then 2") <|> (char '3' <?> "three")
