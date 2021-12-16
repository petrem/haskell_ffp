module Main where

import Data.Maybe (isJust)
import Data.Monoid
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

import Cipher


main :: IO ()
main = do
  args <- getArgs
  if checkArgs args
  then do
    case (args !! 0, args !! 1) of
      ("code", "caesar") -> do
        let result = caesar (read (args !! 2)) (args !! 3)
        putStrLn result
      ("decode", "caesar") -> do
        let result = uncaesar (read (args !! 2)) (args !! 3)
        putStrLn result
      ("code", "vigenere") -> do
        let result = vigenère (args !! 2) (args !! 3)
        putStrLn result
      ("decode", "vigenere") -> do
        let result = unvigenère (args !! 2) (args !! 3)
        putStrLn result
      _ -> do
       putStrLn "Ooops!"
       exitFailure
  else do
    putStrLn "Usage: {code|decode} {caesar|vigenere} key message"
    exitFailure
  where
    checkArgs :: [String] -> Bool
    checkArgs args = getAll . mconcat . map (All . ($ args)) $ checkers

    checkers = [checkNumberOfArgs, checkAction, checkCipher, checkKey]

    checkNumberOfArgs as = length as == 4
    checkAction as = as !! 0 `elem` ["code", "decode"]
    checkCipher as = as !! 1 `elem` ["caesar", "vigenere"]
    checkKey as | as !! 1 == "caesar" = isJust (readMaybe (as !! 2) :: Maybe Int)
                | as !! 1 == "vigenere" = not (null (as !! 2))
                | otherwise = False

