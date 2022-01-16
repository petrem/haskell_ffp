import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Checkers
import Test.Hspec.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Moi


main :: IO ()
main = do
  hspec $ do
    describe "Moi" $ do
      it "fmap increment over Moi" $ do
        let f = (+1) <$> Moi (\s -> (0, s))
        runMoi f 0 `shouldBe` (1, 0)
      it "lift over Moi" $ do
        let g = (+) <$> Moi (\s -> (1, s+1)) <*> Moi (\s -> (1, s+10))
        runMoi g 5 `shouldBe` (2, 16)
      it "bind over Moi" $ do
        let h = Moi (\s -> (3, 3:s)) >>= \a -> Moi (\s -> ((replicate a 'a'), 4:s))
        runMoi h [1,2] `shouldBe` ("aaa", [4, 3, 1, 2])
      -- testBatch (functor (undefined :: Moi String HappyThreeFriends))
      -- testBatch (applicative (undefined:: Moi String HappyThreeMonoids))
      -- testBatch (monad (undefined :: Moi String HappyThreeFriends))
    describe "get" $ do
      it "returns state as result" $ do
        runMoi get "curryIsAmaze" `shouldBe` ("curryIsAmaze", "curryIsAmaze")
    describe "put" $ do
      it "sets State with a () value" $ do
        runMoi (put "blah") "woot" `shouldBe` ((),"blah")
    describe "exec" $ do
      it "evaluates a state computation with the given initial state and return the final state, discarding the final value" $ do
        exec (put "wilma") "daphne" `shouldBe` "wilma"
        exec get "scooby papu" `shouldBe` "scooby papu"
    describe "eval" $ do
      it "evaluates a state computation with the given initial state and return the final value, discarding the final state." $ do
        eval get "bunnicula" `shouldBe` "bunnicula"
        eval (put "wilma") "daphne" `shouldBe` ()
    describe "modify" $ do
      let f = modify (+1)
      it "applies an action within the state and updates with result" $ do
        runMoi f 0 `shouldBe` ((),1)
      it "can be sequentially composed with >>" $ do
        runMoi (f >> f) 0 `shouldBe` ((),2)

type HappyThreeFriends = (Int, String, Char)
type HappyThreeMonoids = ([Int], String, [Char])

happyThreeFriends :: HappyThreeFriends
happyThreeFriends = undefined
