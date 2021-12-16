import Control.Monad (forever)
import Data.Either (isRight)


type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid =  NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = forever $ do
  putStr "Gimme a name: "
  name <- getLine
  putStr "Gimme an age: "
  ageString <- getLine
  let age = read ageString :: Age
  let couldBePerson = mkPerson name age
  if isRight couldBePerson then
    putStrLn $ "Got: " ++ (let (Right person) = couldBePerson in show person)
  else
    putStrLn (let (Left invalidMsg) = couldBePerson in show invalidMsg)
