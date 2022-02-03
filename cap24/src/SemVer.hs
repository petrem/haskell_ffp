{-# LANGUAGE OverloadedStrings #-}
module SemVer where

import Control.Applicative
import Text.Read (readMaybe)
import Text.Trifecta


data Label =
  NonNumLabel String
  | NumLabel Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer

newtype PreRelease = PreRelease [Label] deriving (Eq, Show)

-- | Convenience PreRelease maker
-- | Converts string labels containing integers to Integer.
prerelease :: [String] -> PreRelease
prerelease ls = PreRelease [makeLabel l | l <- ls]
  where makeLabel l = maybe (NonNumLabel l) NumLabel (readMaybe l)

type Metadata = [String]

data SemVer = SemVer Major Minor Patch PreRelease Metadata deriving (Eq, Show)

-- | SemVer maker for releases
semver :: Major -> Minor -> Patch -> SemVer
semver ma mi pa = SemVer ma mi pa (prerelease []) []

-- | SemVer maker for releases with metadata
semverMeta :: Major -> Minor -> Patch -> Metadata -> SemVer
semverMeta ma mi pa = SemVer ma mi pa (prerelease [])

-- | SemVer maker for pre-releases
semverPre :: Major -> Minor -> Patch -> [String] -> SemVer
semverPre ma mi pa rs = SemVer ma mi pa (prerelease rs) []

-- | SemVer maker for pre-releases with metadata
semverPreMeta :: Major -> Minor -> Patch -> [String] -> Metadata -> SemVer
semverPreMeta ma mi pa rs = SemVer ma mi pa (prerelease rs)


instance Ord SemVer where
  (SemVer ma1 mi1 pa1 pre1 _) `compare` (SemVer ma2 mi2 pa2 pre2 _) =
    mconcat [ ma1 `compare` ma2
            , mi1 `compare` mi2
            , pa1 `compare` pa2
            , pre1 `compare` pre2
            ]

instance Ord Label where
  (NumLabel n1) `compare` (NumLabel n2) = n1 `compare` n2
  (NonNumLabel l1) `compare` (NonNumLabel l2) = l1 `compare` l2
  (NumLabel _) `compare` (NonNumLabel _) = LT
  (NonNumLabel _) `compare` (NumLabel _) = GT

instance Ord PreRelease where
  (PreRelease []) `compare` (PreRelease []) = EQ
  (PreRelease []) `compare` _ = GT
  _ `compare` (PreRelease []) = LT
  (PreRelease xs) `compare` (PreRelease ys) = xs `compare` ys


-- | Parser for SemVer as specified at https://semver.org
parseSemVer :: (Monad f, CharParsing f) => f SemVer
parseSemVer = do
  (ma, mi, pa) <- parseVersion
  pre <- option [] (char '-' >> parseLabels)
  meta <- option [] (char '+' >> dotSep identifier)
  eof
  return $ SemVer ma mi pa (PreRelease pre) meta

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
