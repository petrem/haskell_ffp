module Text.Integrals where

import Control.Applicative
import Text.Trifecta

import Text.Naturals

parseIntegral :: Integral a => Parser a
parseIntegral = _parseIntegral parseNatural'

parseIntegral0 :: Integral a => Parser a
parseIntegral0 = _parseIntegral parseNatural0'

_parseIntegral :: Integral a => Parser a -> Parser a
_parseIntegral naturalFlavorParser = do
  neg <- try (char '-') <|> pure '\NUL'
  num <- naturalFlavorParser
  case neg of '-' -> return $ negate num
              _   -> return num
