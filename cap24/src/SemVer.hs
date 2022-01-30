{-# LANGUAGE OverloadedStrings #-}
module SemVer where

import Control.Applicative
import Text.Trifecta



data Label =
  NonNumLabel String
  | NumLabel Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type PreRelease = [Label]
type Metadata = [String]

data SemVer = SemVer Major Minor Patch PreRelease Metadata deriving (Eq, Show)

-- | Parser for SemVer as specified at https://semver.org
parseSemVer :: (Monad f, CharParsing f) => f SemVer
parseSemVer = do
  (ma, mi, pa) <- parseVersion
  pre <- option [] (char '-' >> parseLabels)
  meta <- option [] (char '+' >> dotSep identifier)
  eof
  return $ SemVer ma mi pa pre meta

parseVersion :: (Monad f, CharParsing f) => f (Major, Minor, Patch)
parseVersion = do
  major <- numericIdentifier
  _ <- char '.'
  minor <- numericIdentifier
  _ <- char '.'
  patch <- numericIdentifier
  return (major, minor, patch)

parseLabels :: (Monad f, CharParsing f) => f [Label]
parseLabels = dotSep parseLabel

parseLabel :: (Monad f, CharParsing f) => f Label
parseLabel = NumLabel <$> try numericIdentifier <|> NonNumLabel <$> identifier

numericIdentifier :: (Monad f, CharParsing f) => f Integer
numericIdentifier = read <$> (try (nonNullDigit >>= \d -> (d:) <$> many digit) <|> string "0")

identifier :: (Monad f, CharParsing f) => f String
identifier = some $ alphaNum <|> char '-'

dotSep :: (Monad f, CharParsing f) => f a -> f [a]
dotSep p = p `sepBy` char '.'

nonNullDigit :: CharParsing m => m Char
nonNullDigit = oneOf ['1'..'9']
