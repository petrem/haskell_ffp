module Cap12 where

import Data.Bool
import Data.Char
import Data.Maybe


notThe :: String -> Maybe String
notThe s = bool (Just s) Nothing (map toLower s == "the")

words' :: String -> [String]
words' = go []
  where go :: String -> String -> [String]
        go acc "" = [acc]
        go "" (' ':xs) = go "" xs
        go acc (' ':xs) = acc : go "" xs
        go acc (x:xs) = go (acc ++ [x]) xs

replaceThe :: String -> String
replaceThe = go []
   where
     go acc "" = acc
     go acc (x:xs) = case x of ' ' -> replace (notThe acc) ++ ' ':go "" xs
                               _   -> go (acc ++ [x]) xs
     replace Nothing = "a"
     replace (Just w) = w

replaceThe' :: String -> String
replaceThe' = unwords . map (replace . notThe) . words
  where replace w = case w of Just w' -> w'
                              Nothing -> "a"

wordCount :: String -> Integer
wordCount = go 0 ""
  where go n acc "" = n + wc acc
        go n acc (x:xs) = case x of ' ' -> go (n + wc acc) "" xs
                                    _   -> go n (acc ++ [x]) xs
        wc "" = 0
        wc _ = 1

countTheAfterVowel :: String -> Integer
countTheAfterVowel = go 0 "" False
  where go :: Integer -> String -> Bool -> String -> Integer
        go n acc isBefore "" = n + countBefore isBefore acc
        go n acc isBefore (x:xs) = case x of ' ' -> go (n + countBefore isBefore acc) "" (vowely acc) xs
                                             _   -> go n (acc ++ [x]) isBefore xs

        countBefore :: Bool -> String -> Integer
        countBefore False _ = 0
        countBefore True "the" = 1
        countBefore _ _ = 0

        vowely "" = False
        vowely (y:_) = toLower y `elem` "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go 0 "" False
  where go :: Integer -> String -> Bool -> String -> Integer
        go n acc wasThe "" = n + countAfterThe wasThe acc
        go n acc wasThe (x:xs) = case x of ' ' -> go (n + countAfterThe wasThe acc) "" (isNothing . notThe $ acc) xs
                                           _   -> go n (acc ++ [x]) wasThe xs

        countAfterThe :: Bool -> String -> Integer
        countAfterThe False _ = 0
        countAfterThe True acc = bool 0 1 (vowely acc)

        vowely "" = False
        vowely (y:_) = toLower y `elem` "aeiou"


countVowels :: String -> Int
countVowels = length . filter (`elem` "aeiou") . map toLower


newtype Word' = Word' String deriving (Eq, Show)

vowels :: [Char]
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord w = let (v, c) = counts w in if v > c then Nothing else Just $ Word' w
  where
    counts :: String -> (Integer, Integer)
    counts = foldr (\l (v', c') -> if l `elem` vowels then (v'+1, c') else (v', c'+1)) (0,0)


data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)


natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x | x < 0 = Nothing
               | otherwise = Just $ iterate Succ Zero !! fromInteger x

isJust' :: Maybe a -> Bool
isJust' Nothing = False
isJust' _ = True

isNothing' :: Maybe a -> Bool
isNothing' Nothing = True
isNothing' _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe' :: a -> Maybe a -> a
fromMaybe' a = mayybee a id

listToMaybe :: [a] -> Maybe a
listToMaybe = foldr (const . Just) Nothing

maybeToList :: Maybe a -> [a]
maybeToList = maybe [] (:[])

catMaybes :: [Maybe a] -> [a]
catMaybes = map fromJust . filter isJust'

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr go (Just [])
  where go b (Just z) = fmap (:z) b
        go _ Nothing = Nothing

lefts' :: [Either a b] -> [a]
lefts' = foldr go []
  where go (Left a) = (a:)
        go _ = id

rights' :: [Either a b] -> [b]
rights' = foldr go []
  where go (Right b) = (b:)
        go _ = id

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([], [])
  where go (Left a) (as, bs) = (a:as, bs)
        go (Right b) (as, bs) = (as, b:bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' = either' (const Nothing) . (Just .)


myIterate :: (a -> a) -> a -> [a]
myIterate f x = go []
  where
    go [] = x : go [x]
    go r@(y:_) = f y : go (f y : r)

myIterate' :: (a -> a) -> a -> [a]
myIterate' f x = x : myIterate' f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = go (f x)
  where go Nothing = []
        go (Just (value, nextSeed)) = value : go (f nextSeed)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\y -> Just (y, f y))


data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x (Node l y r) | x < y = Node (insert' x l) y r
                       | otherwise = Node l y (insert' x r)

instance Functor BinaryTree where
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)


unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = go (f x)
  where
    go Nothing = Leaf
    go (Just (lSeed, value, rSeed)) = Node (go (f lSeed)) value (go (f rSeed))

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold go n
  where go 0 = Nothing
        go x = Just (x-1, n-x, x-1)
