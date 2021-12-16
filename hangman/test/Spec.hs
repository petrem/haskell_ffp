import Test.Hspec
import Test.Hspec.QuickCheck -- for prop
import Test.QuickCheck

import Hangman


main :: IO ()
main = hspec $ do
  describe "FillInCharacter" $ do
    it "fill in single matching character" $ do
      fillInCharacter (freshPuzzle "abc") 'a' `shouldBe` Puzzle "abc" [Just 'a', Nothing, Nothing] "a"
    it "fill in multiple matching characters" $ do
      fillInCharacter (freshPuzzle "aba") 'a' `shouldBe` Puzzle "aba" [Just 'a', Nothing, Just 'a'] "a"
    it "ignore non-maching character" $ do
      fillInCharacter (freshPuzzle "abc") 'x' `shouldBe` Puzzle "abc" [Nothing, Nothing, Nothing] "x"
  describe "handleGuesses" $ do
    it "adds character to attempts list when part of word" $ do
      puzzle <- handleGuess (freshPuzzle "abc") 'a'
      puzzle `shouldBe` Puzzle "abc" [Just 'a', Nothing, Nothing] "a"
    it "adds character to attempts list when NOT part of word" $ do
      puzzle <- handleGuess (freshPuzzle "abc") 'x'
      puzzle `shouldBe` Puzzle "abc" [Nothing, Nothing, Nothing] "x"
    it "skips call to fillInCharacter when character already guessed" $ do
      puzzle <- handleGuess (Puzzle "abc" [Just 'a', Nothing, Nothing] "a") 'a'
      puzzle `shouldBe` Puzzle "abc" [Just 'a', Nothing, Nothing] "a"
    --prop "does not change fully guessed puzzle" prop_handleGuesses_doesNotChangeFullyGuessedPuzzle


prop_handleGuesses_doesNotChangeFullyGuessedPuzzle :: Char -> IO Bool
prop_handleGuesses_doesNotChangeFullyGuessedPuzzle c = do
  puzzle <- handleGuess fullPuzzle c
  return $ puzzle == fullPuzzle
  where fullPuzzle = Puzzle "abc" [Just 'a', Just 'b', Just 'c'] "abc"

