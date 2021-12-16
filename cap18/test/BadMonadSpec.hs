module BadMonadSpec where

import Test.Hspec
import Test.Hspec.Checkers
import Test.Hspec.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import BadMonad


badMonadTests :: IO ()
badMonadTests = do
  hspec $ do
    describe "Bad Monad" $ do
      let trigger :: CountMe (Int, String, Int)
          trigger = undefined
      testBatch $ functor trigger
      testBatch $ applicative trigger
      testBatch $ monad trigger
