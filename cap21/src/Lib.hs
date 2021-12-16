module Lib where

import Control.Applicative
import Control.Monad (replicateM)

import qualified Data.ByteString.Lazy as B
import Network.Wreq

import Test.QuickCheck
import Test.QuickCheck.Checkers

--


data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = fetchFn query >>=  traverse makeIoOnlyObj . traverse decodeFn


--

urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response B.ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response B.ByteString]
traversedUrls = traverse get urls


--

listTraverse :: Applicative f => (a -> f b) -> [a] -> f [b]
listTraverse _ [] = pure []
listTraverse f (x:xs) = liftA2 (:) (f x) (listTraverse f xs)

listSequenceA :: Applicative f => [f a] -> f [a]
listSequenceA [] = pure []
listSequenceA (u:us) = (:) <$> u <*> listSequenceA us

listSequenceA' :: Applicative f => [f a] -> f [a]
listSequenceA' = foldr (liftA2 (:)) (pure [])


--

maybeTraverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
maybeTraverse _ Nothing = pure Nothing
maybeTraverse f (Just x) = Just <$> f x

maybeSequenceA :: Applicative f =>  Maybe (f a) -> f (Maybe a)
maybeSequenceA Nothing = pure Nothing
maybeSequenceA (Just u) = Just <$> u

--

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

--

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a
  sequenceA (Constant a) = pure $ Constant a

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

--sequenceA :: (Traversable t,         Applicative f) => t       (f a) -> f (t       a)
--sequenceA :: (Traversable (Const c), Applicative f) => Const c (f a) -> f (Const c a)

--traverse :: (Traversable t,       Applicative f) => (a -> f b) -> t       a -> f (t b)
--traverse :: (Traversable Const c, Applicative f) => (a -> f b) -> Const c a -> f (Const c b)

--

data Opcional a = Nada
                | Algo a
                deriving (Eq, Show)

instance Functor Opcional where
  fmap _ Nada = Nada
  fmap f (Algo x) = Algo (f x)

instance Foldable Opcional where
  foldMap _ Nada = mempty
  foldMap f (Algo x) = f x

instance Traversable Opcional where
  sequenceA Nada = pure Nada
  sequenceA (Algo u) = Algo <$> u

  traverse _ Nada = pure Nada
  traverse f (Algo x) = Algo <$> f x

instance Arbitrary a => Arbitrary (Opcional a) where
  arbitrary = frequency [(1, pure Nada), (9, Algo <$> arbitrary)]

instance Eq a => EqProp (Opcional a) where
  (=-=) = eq

--

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a l) = f a <> foldMap f l

instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons u l) = Cons <$> u <*> sequenceA l

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized $ \n -> do
    k <- choose (0, n)
    fromFoldable <$> replicateM k arbitrary

fromFoldable :: Foldable t => t a -> List a
fromFoldable = foldr Cons Nil

instance Eq a => EqProp (List a) where
  xs =-= ys = listTake 200 xs `eq` listTake 200 ys

listTake :: Int -> List a -> List a
listTake 0 _ = Nil
listTake _ Nil = Nil
listTake n (Cons a l) = Cons a (listTake (n-1) l)

--

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c
  foldr f z (Three _ _ c) = f c z

instance Traversable (Three a b) where
  sequenceA (Three a b u) = Three a b <$> u
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

--

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b
  foldr f z (Pair _ b) = f b z

instance Traversable (Pair a) where
  sequenceA (Pair a u) = Pair a <$> u
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

--

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'
  foldr f z (Big _ b b') = f b (f b' z)

instance Traversable (Big a) where
  sequenceA (Big a u u') = Big a <$> u <*> u'
  traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq


--

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b1 b2 b3) = f b1 <> f b2 <> f b3
  foldr f z (Bigger _ b1 b2 b3) = f b1 (f b2 (f b3 z))

instance Traversable (Bigger a) where
  sequenceA (Bigger a u1 u2 u3) = Bigger a <$> u1 <*> u2 <*> u3
  traverse f (Bigger a b1 b2 b3) = Bigger a <$> f b1 <*> f b2 <*> f b3

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

--

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary


instance ( Applicative n
         , Testable (n Property)
         , Eq a
         , Eq (n a) , EqProp a
         ) => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S u a) = S (f <$> u) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S u a) = foldMap f u <> f a

instance Traversable n => Traversable (S n) where
  sequenceA (S u a) = S <$> sequenceA u <*> a
  traverse f (S u a) = S <$> traverse f u <*> f a

--

data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

-- with toList

-- instance Foldable Tree where
--   foldMap f = foldMap f . treeToList
--   foldr f z = foldr f z . treeToList

-- recursive
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a ) = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

  foldr _ z Empty = z
  foldr f z (Leaf a) = f a z
  foldr f z (Node l a r) = foldr f (f a (foldr f z r)) l

instance Traversable Tree where
  sequenceA Empty = pure Empty
  sequenceA (Leaf u) = Leaf <$> u
  sequenceA (Node ul u ur) = Node <$> sequenceA ul <*> u <*> sequenceA ur

  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized $ \n -> do
    as <- replicateM n arbitrary
    return $ treeFromList as

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

treeFromList :: [a] -> Tree a
treeFromList [] = Empty
treeFromList [x] = Leaf x
treeFromList (x:xs) = Node Empty x (treeFromList xs)

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Leaf x) = [x]
treeToList (Node l x r) = treeToList l ++ [x] ++ treeToList r
