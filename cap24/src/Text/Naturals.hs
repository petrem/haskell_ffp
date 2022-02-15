module Text.Naturals where

import Control.Applicative
import Data.Char (digitToInt)
import Text.Trifecta


-- parseNatural :: Parser Integer
-- parseNatural = do
-- --  digits <- parseNonNullDigit >>= (<$> many parseDigit) . (:)
--   digits <- (:) <$> parseNonNullDigit <*>  many parseDigit
--   return $ read digits
--parseNatural :: Parser Integer
--parseNatural = foldl (flip (go . fromIntegral . digitToInt)) 0 <$> ((:) <$> parseNonNullDigit <*> many parseDigit)
--  where go a b = a + b * 10

parseNatural :: Parser Int
parseNatural =
  getIntegral . foldMap (B10R 1 . digitToInt) <$>
  ((:) <$> parseNonNullDigit <*> many parseDigit)

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

parseNonNullDigit :: Parser Char
parseNonNullDigit = oneOf ['1'..'9']

data B10R a = B10R { nCount :: Word
                   , getIntegral :: a
                   } deriving (Eq, Show)

instance Num a => Semigroup (B10R a) where
  B10R n1 a <> B10R n2 b = B10R (n1 + n2) $ a * 10^n2 + b

instance Num a => Monoid (B10R a) where
  mempty = B10R 0 0

instance Foldable B10R where
  foldr f z (B10R _ a) = f a z

parseDigit' :: Integral a => Parser (B10R a)
parseDigit' = B10R 1 . fromIntegral . digitToInt <$> oneOf ['0'..'9']

parseNonNullDigit' :: Integral a => Parser (B10R a)
parseNonNullDigit' = B10R 1 . fromIntegral . digitToInt <$> oneOf ['1'..'9']

parseNullDigit' :: Integral a => Parser (B10R a)
parseNullDigit' = B10R 1 . fromIntegral . digitToInt <$> char '0'

mMany :: (Alternative f, Monoid m) => f m -> f m
mMany v = many_v
  where
    many_v = some_v <|> pure mempty
    some_v = liftA2 (<>) v many_v

-- | parse natural numbers that aren't left-padded with zeros
-- thus 123, 4 or 0 are valid
-- but 0123, 04 or 00 are not
parseNatural' :: Integral a => Parser a
parseNatural' = getIntegral <$> (parseNullDigit' <|> (parseNonNullDigit' <> mMany parseDigit'))

-- | parse natural numbers that may be left-padded with zeros
parseNatural0' :: Integral a => Parser a
parseNatural0' = getIntegral <$> mMany parseDigit'

