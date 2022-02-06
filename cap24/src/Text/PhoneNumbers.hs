module Text.PhoneNumbers where

import Control.Applicative
import Text.Trifecta

import Text.Naturals


-- aka area code
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int
data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)


phoneNumberExamples = ["123-456-7890", "1234567890", "(123) 456-7890", "1-123-456-7890"]

parsePhone :: Parser PhoneNumber
parsePhone = do
  skipOptional $ string "1-"
  area <- paranthesized (sizedNatural 3) <* skipMany (char ' ') <|> sizedNatural 3
  skipOptional $ char '-'
  exc <- sizedNatural 3
  skipOptional $ char '-'
  line <- sizedNatural 4
  return $ PhoneNumber area exc line

paranthesized :: Parser a -> Parser a
paranthesized p = skipOptional (char '(') *> p <* skipOptional (char ')')

sizedNatural :: Int -> Parser Int
sizedNatural n = getIntegral <$> parseNonNullDigit' <> mThisMany (n-1) parseDigit'

-- | combinator similar to `some` but taking exactly `size` and monoidally accumulating
-- | the result, instead of returning a list
mThisMany :: (Applicative f, Monoid m) => Int -> f m -> f m
mThisMany size v = some_v size
  where
    some_v n | n <= 0    = pure mempty
             | otherwise = liftA2 (<>) v (some_v $ n - 1)
