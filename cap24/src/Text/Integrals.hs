module Text.Integrals where

import Control.Applicative
import Text.Trifecta

import Text.Naturals

parseIntegrals :: Parser Integer
parseIntegrals = do
  neg <- try (char '-') <|> pure '\NUL'
  num <- parseNatural'
  case neg of '-' -> return $ negate num
              _ -> return num
