{-# LANGUAGE InstanceSigs #-}

module OhMyReader where

import Control.Applicative
import Data.Char


beep :: Num a => a -> a
beep = (+10)

boop :: Num a => a -> a
boop = (*5)

bip :: Num a => a -> a
bip = beep . boop


cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  a <- cap
  b <- rev
  return (a, b)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= \a -> rev >>= \b -> return (a, b)

addOne :: Num a =>  a -> a
addOne = do
  a <- (+1)
  return a

addOne' :: Num a => a -> a
addOne' = (+1) >>= (\a -> return a)

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = fmap f fa <*> fb

newtype Reader r a = Reader {runReader :: r -> a}

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f ra = Reader $ f . runReader ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const
  --pure a = Reader $ (\_ -> a)

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader rab <*> Reader ra = Reader $ \r -> rab r (ra r)


instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRrb =
    Reader $ \r -> runReader (aRrb (ra r)) r

join :: Reader r (Reader r a) -> Reader r a
--join (Reader rRra) = Reader $ \r -> runReader (rRra r) r
join rrRra = rrRra >>= id

--


newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)


data Person = Person {
  humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

-- Dog :: DogName -> Address -> Dog

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

peter :: Person
peter = Person (HumanName "Peter") (DogName "Snorri") (Address "I'm not really a dog but a cat so my address is wherever I like to be")

papu :: Dog
papu = Dog {dogsName = DogName "Papu", dogsAddress = Address "Austin"}

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

(<$->>) :: (a -> b) -> (r -> a) -> (r -> b)
--(<$->>) = (<$>)
--f <$->> g = f . g
f <$->> g = \r -> f (g r)

(<*->>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
--(<*->>) = (<*>)
f <*->> g = \r -> f r (g r)

getDogR' :: Person -> Dog
getDogR' = Dog <$->> dogName <*->> address

getDogRM :: Person -> Dog
-- getDogRM = do
--   name <- dogName
--   addy <- address
--   return $ Dog name addy
--getDogRM = dogName >>= (\name -> address >>= (\addy -> return $ Dog name addy))
--getDogRM = dogName >>= \name -> address >>= return . Dog name
getDogRM = dogName >>= \name -> Dog name <$> address

dogNameR :: Reader Person DogName
dogNameR = Reader dogName

addressR :: Reader Person Address
addressR = Reader address

getDogRM' :: Reader Person Dog
getDogRM' = do
  a <- addressR
  d <- dogNameR
  -- Fascinating, the following workds like a charm (a charm that actually works):
  -- Reader (\_ -> Dog d a)
  return $ Dog d a

