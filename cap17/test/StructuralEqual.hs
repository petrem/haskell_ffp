module StructuralEqual where

import Control.Applicative
import Data.Monoid

import Test.Hspec
import Test.Hspec.Checkers
import Test.Hspec.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Lib


-- just playing with applicatives
-- "structual equality", "monoidal associative", are loose terms invented ad-hoc

-- main :: IO ()
-- main = do
--   hspec $ do
--     describe "Identity" $ do
--       prop "Monoidal Associative" (prop_MonoidalAssociative :: Identity String -> Bool)


-- prop_MonoidalAssociative :: (Applicative f, Eq (f a)) => f (b -> c) -> f (a -> b) -> f a -> Bool
-- prop_MonoidalAssociative 
