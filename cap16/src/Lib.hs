{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, frequency, vectorOf, sized)

--

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . (("123"++) . show)) ioi
    in fmap (*3) changed

--

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity i) = Identity (f i)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

--

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair p1 p2) = Pair (f p1) (f p2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    v1 <- arbitrary
    v2 <- arbitrary
    return (Pair v1 v2)

--

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

--

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

--

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return (Three' a b b')

--

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

--

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return (Four' a1 a2 a3 b)


--

data Possibly a = LolNope
                | Yeppers a
                deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    frequency [(2, return LolNope), (8, return $ Yeppers a)]

--

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(3, return $ First a), (7, return $ Second b)]

--

data Sum' b a = First' a
             | Second' b

instance Functor (Sum' e) where
  fmap f (First' a) = First' (f a)
  fmap _ (Second' b) = Second' b

--

data Company a c b = DeepBlue a c
                   | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

--

data More b a = L a b a
              | R b a b
              deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


--

data Quant a b = Finance
               | Desk a
               | Bloor b
               deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(2, return Finance), (4, return $ Desk a), (4, return $ Bloor b)]

--

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

instance Arbitrary a => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary

--

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' b)) = Flip $ K' (f b)

instance Arbitrary b => Arbitrary (Flip K' a b) where
  arbitrary = do
    a <- arbitrary
    return $ Flip (K' a)

--

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary


--

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut wrapper) = LiftItOut $ fmap f wrapper

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
  arbitrary = LiftItOut <$> arbitrary

--

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (f <$> x) (f <$> y)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
  arbitrary = do
    fa <- arbitrary
    ga <- arbitrary
    return $ DaWrappa fa ga

--

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (f <$> gb)

instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
  arbitrary = do
    fa <- arbitrary
    fb <- arbitrary
    return $ IgnoringSomething fa fb

--

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) => Arbitrary (Notorious g o a t) where
  arbitrary = do
    go <- arbitrary
    ga <- arbitrary
    gt <- arbitrary
    return $ Notorious go ga gt

--

--You’ll need to use recursion:

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Foldable List where
  -- foldr :: (a -> b -> b) -> b -> List a -> b
  foldr _ b Nil = b
  foldr f b (Cons a l) = f a (foldr f b l)

instance Traversable List where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  -- sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = foldr (\a b -> Cons <$> a <*> b) (pure Nil)

-- YES!!


arbitraryList :: Arbitrary a => Gen (List a)
arbitraryList =
 sized $
   \n -> do
     k <- choose (0, n)
     -- construct a List of (Gen a) of length k
     let l = foldr Cons Nil [arbitrary | _ <- [1..k]]
     sequenceA l

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = arbitraryList


--

data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
                deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

-- does this make sense?
instance Foldable GoatLord where
  foldr _ b NoGoat = b
  foldr f b (OneGoat a) = f a b
  foldr f b (MoreGoats g1 g2 g3) = foldr f (foldr f (foldr f b g3) g2) g1


instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary = sized arbitrarySizedGoatLord

arbitrarySizedGoatLord :: Arbitrary a => Int -> Gen (GoatLord a)
arbitrarySizedGoatLord 0 = return NoGoat
arbitrarySizedGoatLord 1 = OneGoat <$> arbitrary
arbitrarySizedGoatLord n = do
  let m = n `div` 4
  g1 <- arbitrarySizedGoatLord m
  g2 <- arbitrarySizedGoatLord m
  g3 <- arbitrarySizedGoatLord m
  return $ MoreGoats g1 g2 g3

--

-- You’ll use an extra functor for this one, although your solution might do it monomorphically without using fmap. Keep in mind that you will probably not be able to validate this one in the usual manner. Do your best to make it work:
-- TODO: what is this other functor? What's that "monomorphically without using fmap"?

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  --fmap f (Read g) = Read (f . g)
  -- or
  fmap f (Read g) = Read $ fmap f g

-- TODO: the string in Print and the function in Read are not so arbitrary
-- could that be improved?
instance(Arbitrary a, Show a, Read a) => Arbitrary (TalkToMe a) where
  arbitrary = do
    a <- arbitrary
    frequency [(2, return Halt), (4, return $ Print (show a) a), (4, return $ Read read)]

-- for using ``prop`` in testing we need Show
instance Show a => Show (TalkToMe a) where
  show Halt = "Halt"
  show (Print s a) = "Print " ++ s ++ " " ++ show a
  show (Read _) = "Read <function>"

isHalt :: TalkToMe a -> Bool
isHalt Halt = True
isHalt _ = False

maybePrint :: TalkToMe a -> Maybe (String, a)
maybePrint (Print s a) = Just (s, a)
maybePrint _ = Nothing

maybeApplyRead :: String -> TalkToMe a -> Maybe a
maybeApplyRead s (Read f) = Just (f s)
maybeApplyRead _ _ = Nothing
