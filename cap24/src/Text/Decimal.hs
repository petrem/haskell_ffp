{-# LANGUAGE OverloadedStrings #-}

module Text.Decimal where

import Control.Applicative
import Data.Maybe

import Text.Trifecta

import Text.Fractions

data QPlus = Natural Integer | Frac Rational deriving (Eq, Show)


badDecimals = ["1", "a", "10.a", "10."]
goodDecimals = ["1.1", "1.10", "0.1", ".1"]

goodQs = concat [goodFracs, goodDecimals, ["1", "001"]]


nullDigit :: Parser Char
nullDigit = char '0' 

-- | gobble up any digits, discard the trailing 0s
parseDecimals :: Parser String
parseDecimals =
  manyTill digit (try (many nullDigit <* notFollowedBy digit))

  
parseDecimal :: Parser Rational
parseDecimal = do
  int_part <- fromMaybe 0 <$> optional decimal
  _ <- char '.'
  decimals_str <- parseDecimals
  let (decimals_part, n_decimals) =
        if null decimals_str
        then (0, 0)
        else (,) <$> read <*> length $ decimals_str
        
  return $ fromRational $ fromInteger int_part + fromInteger decimals_part / 10.0^^n_decimals 

parseQPlus :: Parser QPlus
parseQPlus = try (Frac <$> parseDecimal) <|> try (Frac <$> parseFraction) <|>(Natural <$> natural)
