module Hangman where

import Control.Monad (forever)
import Data.List (intersperse, partition, sort)
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid
import System.Exit (exitSuccess)
import System.Random (randomRIO)


newtype WordList = WordList [String] deriving (Eq, Show)


data Puzzle = Puzzle { word :: String
                     , status :: [Maybe Char]
                     , guessed :: [Char]
                     }
              deriving Eq

instance Show Puzzle where
  show (Puzzle _ discovered guessed') =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: "
    ++ sort guessed'


allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 7

maxWordLength :: Int
maxWordLength = 7

goodWord :: String -> Bool
goodWord w = getAll . mconcat . map (All . ($ w)) $ [
    gameLength
  , vowelBallanced
  ]
  where gameLength w' =
          let l = length (w' :: String)
          in l >= minWordLength && l <= maxWordLength
        vowelBallanced w' =
          let (v, c) = partition (`elem` "aeiou") w'
          in length c - length v < 3


gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList $ filter goodWord aw

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (fmap (const Nothing) w) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = c `elem` w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed p = flip elem $ guessed p

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word' filledInSoFar s) c = Puzzle word' newFilledInSoFar (c : s)
  where zipper guessed' wordChar guessChar =
          if wordChar == guessed'
          then Just wordChar
          else guessChar
        newFilledInSoFar = zipWith (zipper c) word' filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case ( charInWord puzzle guess
       , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
               \ character, pick \
               \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
               \ word, filling in the word\
               \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
               \ the word, try again."
      return (fillInCharacter puzzle guess)

incorrectGuesses :: Puzzle -> Int
incorrectGuesses (Puzzle _ filledInSoFar guessedSoFar) =
  length $ filter (\c -> Just c `notElem` filledInSoFar) guessedSoFar

gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle wordToGuess _ _) = do
  let left = 7 - incorrectGuesses p
  if left <= 0 then do
    putStrLn "You lose!"
    putStrLn $
      "The word was: " ++ wordToGuess
    exitSuccess
  else do
    putStrLn $ "Guesses left: " ++ show left


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
  then do
    putStrLn "You win!"
    exitSuccess
  else
    return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ ->
      putStrLn "Your guess must\
               \ be a single character"
