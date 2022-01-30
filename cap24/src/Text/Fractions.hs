{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))

import Text.Trifecta


badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

goodFracs = ["1/2", "2/1"]


parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of 0 -> fail "/0"
                      _ -> return $ numerator % denominator


runFracs :: IO ()
runFracs = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' badFraction
  print $ parseFraction' alsoBad
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
