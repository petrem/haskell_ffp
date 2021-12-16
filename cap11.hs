-- allow automatically deriving of user-defined type classes
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- allow instances for types that aren't of the form T a b c.... (e.g. lists, tuples)
{-# LANGUAGE FlexibleInstances #-}

module Cap11 where

import Data.Bool
import Data.Char
import Data.Int
import Data.List (intercalate, unfoldr)


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- instance TooMany Num a => a where
--   tooMany n = n > 5

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

newtype Cows = Cows Int deriving (Eq, Show)

instance TooMany Cows where
  tooMany (Cows n) = n > 20

instance TooMany (Int, String) where
  tooMany (n, _) = n > 20

instance TooMany (Int, Int) where
  tooMany (n, m) = n + m > 20

instance (Num a, Ord a, TooMany a) => TooMany (a, a) where
  tooMany (n, m) = n + m > 0

data BigSmall =
         Big Bool
         | Small Bool deriving (Eq, Show)

data NumberOrBool =
         Numba Int8
         | BoolyBool Bool deriving (Eq, Show)

data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth deriving (Eq, Show, Enum, Bounded)

data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String
data Author = Author (AuthorName, BookType) deriving (Show)

type AuthorName' = String
data Author' =
    Fiction' AuthorName'
  | Nonfiction' AuthorName deriving (Eq, Show)

data Expr =
    Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr deriving (Eq, Show)

-- type Number = Int
-- type Add = (Expr, Expr)
-- type Minus = Expr
-- type Mult = (Expr, Expr)
-- type Divide = (Expr, Expr)
-- type Expr = Either Number
--                   (Either Add
--                          (Either Minus
--                                 (Either Mult Divide)
--                          )
--                   )

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show
type Gardener = String
data Garden =
  Garden Gardener FlowerType
  deriving Show

data Garden' = Gardenia' Gardener
             | Daisy' Gardener


data GuessWhat = ChickenButt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a
             | Second b
             deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a
                                       , psecond :: b } deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)
data Animal =
  Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo deriving (Eq, Show)
-- Alternatively
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)



data BinaryTree a = Leaf
  | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x (Node l y r) | x < y = Node (insert' x l) y r
                       | otherwise = Node l y (insert' x r)

instance Functor BinaryTree where
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs@(x:xs') (y:ys') | x == y    = isSubseqOf xs' ys'
                              | otherwise = isSubseqOf xs ys'


capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capiTuplize . words

capiTuplize :: String -> (String, String)
capiTuplize "" = ("", "")
capiTuplize w@(h:t) = (w, toUpper h:t)

capitalizeWord :: String -> String
capitalizeWord = (:) <$> toUpper . head <*> tail

capitalizeWord' :: String -> String
capitalizeWord' "" = ""
capitalizeWord' (c:cs) | isLetter c = toUpper c : cs
                       | otherwise = c:capitalizeWord' cs

capitalizeParagraph :: String -> String
capitalizeParagraph = unsentences . map capitalizeWord' . sentences

sentences :: String -> [String]
sentences = unfoldr nextSentence
  where nextSentence s =
          let (f, r) = span (/= '.') s in
            if f == ""
            then Nothing
            else Just (f, dropWhile (== '.') r)

unsentences :: [String] -> String
unsentences = intercalate "."
