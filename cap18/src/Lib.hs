module Lib where

-- Cap 18 - Monad

--import Control.Applicative
import Control.Monad

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Cow = Cow { name :: String
               , age :: Int
               , weight :: Int
               } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- if Cow's name is Bess,
-- it must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499 then Nothing
  else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)


mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' =
  noEmpty name' >>= \nammy ->
    noNegative age' >>= \agey ->
      noNegative weight' >>= \weighty ->
        weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>= \nammy ->
    noNegative age' >>= \agey ->
      noNegative weight' >>= \weighty ->
        weightCheck (Cow nammy agey weighty)

--

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

--

doNothing :: Monad f => a -> (a -> f a) -> (a -> f a) -> f (a, a)
doNothing n f g = do
  a <- f n
  b <- g a
  pure (a, b)

doNothing' :: Monad f => a -> (a -> f a) -> (a -> f a) -> f (a, a)
doNothing' n f g =
  f n >>= \a ->
            g a >>= \b ->
                      pure (a, b)

-- doNothing'' :: Applicative f => a -> (a -> f a) -> (a -> f a) -> f (a, a)
-- doNothing'' n f g =
--   let a = f n
--   in let b = fmap g a
--      in (\x -> (x, x)) <$> b


f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i + 1)
  else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

--

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure b = Second b
  (First a) <*> _ = First a
  (Second f) <*> u = fmap f u

instance Monad (Sum a) where
  return = pure
  First a >>= _ = First a
  Second b >>= f = f b

--

data Nope a = NopeDotJpg deriving (Show, Eq)

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Functor Nope where
  fmap _ = const NopeDotJpg

instance Applicative Nope where
  pure = const NopeDotJpg
  (<*>) = const . const NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) = const . const NopeDotJpg

--

data BahEither b a = PLeft a
                   | PRight b
                   deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither a b) where
  arbitrary = frequency [(5, PLeft <$> arbitrary), (5, PRight <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (BahEither a b) where
  (=-=) = eq

instance Functor (BahEither b) where
  fmap f (PLeft a) = PLeft (f a)
  fmap _ (PRight b) = PRight b

instance Applicative (BahEither b) where
  pure = PLeft
  -- (BE b (x -> y) -> BE b x -> BE b y
  PLeft f <*> u = fmap f u
  PRight b <*> _ = PRight b

instance Monad (BahEither b) where
  return = pure
  PLeft a >>= f = f a
  PRight b >>= _ = PRight b

--

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

--

data List a = Nil
            | Cons a (List a)
            deriving (Show, Eq)

instance Arbitrary a => Arbitrary (List a) where
  -- sized :: (Int -> Gen (List a)) -> Gen (List a)
  arbitrary = sized $ \n -> do
    k <- choose (0, n)
    fromFoldable <$> sequence [arbitrary | _ <- [1..k]]

instance Eq a => EqProp (List a) where
  xs =-= ys = listTake 200 xs `eq` listTake 200 ys

instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons a l) = f a (foldr f z l)

listTake :: Int -> List a -> List a
listTake 0 _ = Nil
listTake _ Nil = Nil
listTake n (Cons a l) = Cons a (listTake (n-1) l)

listLen :: List a -> Int
listLen = foldr (\_ b -> b + 1) 0

fromFoldable :: Foldable t => t a -> List a
fromFoldable = foldr Cons Nil

instance Functor List where
  -- (1)
  -- fmap _ Nil = Nil
  -- fmap f (Cons a l) = Cons (f a) (fmap f l)
  -- (2)
  -- fmap f = foldr (\a r -> Cons (f a) r) Nil
  -- (3)
  fmap f = foldr (Cons . f) Nil

instance Applicative List where
  pure = flip Cons Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> u = fmap f u <> (fs <*> u)

instance Semigroup (List a) where
  l <> Nil = l
  Nil <> l = l
  Cons a l <> r = Cons a (l <> r)

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons a l >>= f = f a <> (l >>= f)

--

j :: Monad m => m (m a) -> m a
-- j = join
-- (>>=) :: Monad m => m (m a) -> (m a -> m b) -> m b
j u = u >>= id

--

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

--

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- l2 = liftM2
-- l2 f u v = f <$> u <*> v

-- the following resulted after several simplifications, the idea was to obtain first
-- a `m (b->c)` and bind it to a function that applies the "extracted" `(b -> c)` over `m b`
-- a first convoluted variant was -- l2 f u v = (u >>= (return . f)) >>= (`fmap` v)

-- l2 f u v = fmap f u >>= (`fmap` v)
-- after suggestion by ghc:
l2 f u v = u >>= (`fmap` v) . f

--

a :: Monad m => m a -> m (a -> b) -> m b
a u vf = vf >>= (`fmap` u)

--

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = return (:) `ap` f x `ap` meh xs f

--

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
