{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Marsh where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Text.RawString.QQ
import qualified Data.Text as T
import Data.Text (Text)
import Data.Scientific

sectionJson :: ByteString
sectionJson = [r|
{
  "section": {
    "host": "wikipedia.org"
  },
  "whatisit": {
    "red": "intoothandclaw"
  }
}
|]

data TestData =
  TestData
    { section :: Host
    , what :: Color
    }
  deriving (Eq, Show)

newtype Host =
  Host String
  deriving (Eq, Show)

type Annotation = String

data Color
  = Red Annotation
  | Blue Annotation
  | Yellow Annotation
  deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) = TestData <$> v .: "section" <*> v .: "whatisit"
  parseJSON _ = fail "go find an object for TestData"

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "host"
  parseJSON _ = fail "go find an object for Host"

instance FromJSON Color where
  parseJSON (Object v) =
    Red <$> v .: "red" <|> Yellow <$> v .: "yellow" <|> Blue <$> v .: "blue"
  parseJSON _ = fail "go find an object for Host"

mainMarsh = do
  let d :: Maybe TestData
      d = decode sectionJson
  print d


data NumberOrString =
    Numba Integer
  | Stringy Text deriving (Eq, Show)

-- instance FromJSON NumberOrString where
--   parseJSON (Number i) = return $ Numba (truncate i)
--   parseJSON (String s) = return $ Stringy s
--   parseJSON _ = fail "NumberOrString must be number or string"


instance FromJSON NumberOrString where
  parseJSON (Number i) =
    case floatingOrInteger i of
      (Left _) -> fail "Must be integral number"
      (Right integer) -> return $ Numba integer
  parseJSON (String s) = return $ Stringy s
  parseJSON _ = fail "NumberOrString must be number or string"
