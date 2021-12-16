module Cap15 where

import Data.Monoid
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck


data Optional a = Only a | Nada deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> x = x
  x <> Nada = x
  (Only x) <> (Only y) = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend = (<>)


prop_assoc :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
prop_assoc (<**>) a b c = (a <**> (b <**> c)) == ((a <**> b) <**> c)

monoidAssoc :: (Eq m, Monoid m)=> m -> m -> m -> Bool
monoidAssoc = prop_assoc (<>)

data Bull = Fools
          | Twoo
          deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
   _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

testBullMonoid :: IO ()
testBullMonoid = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  putStrLn "monadAssoc"
  quickCheck (ma :: BullMappend)
  putStrLn "monadLeftIdentity"
  quickCheck (mli :: Bull -> Bool)
  putStrLn "monadRightIdentity"
  quickCheck (mri :: Bull -> Bool)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = a <> mempty == a


-- First monoid

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Semigroup (First' a) where
  (First' Nada) <> x = x
  x <> _ = x

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return Nada), (7, return (Only x))]


instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    return $ First' a


testFirstMonoid :: IO ()
testFirstMonoid = do
  quickCheck (monoidAssoc :: First' String -> First' String -> First' String-> Bool)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)


-- Semigroups & Monoids

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc = prop_assoc (<>)

--

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

testTrivial :: IO ()
testTrivial = do
  hspec $ do
    describe "Trivial Semigroup" $ do
      prop "is associative" (semigroupAssoc :: TrivAssoc)
    describe "Trivial Monoid" $ do
      prop "has left identity" (monoidLeftIdentity :: Trivial -> Bool)
      prop "has right identity" (monoidRightIdentity :: Trivial -> Bool)

--

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentAssoc = Identity String -> Identity String -> Identity String -> Bool

testIdentity :: IO ()
testIdentity = do
  hspec $ do
    describe "Identity Semigroup" $ do
      prop "is associative" (semigroupAssoc :: IdentAssoc)
    describe "Identity Monoid" $ do
      prop "has left identity" (monoidLeftIdentity :: Identity String -> Bool)
      prop "has right identity" (monoidRightIdentity :: Identity String -> Bool)

--

data Two a b = Two a b deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two  (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three  (a <> a') (b <> b') (c <> c')

instance (Semigroup a, Semigroup d) => Semigroup (Four a b c d) where
  (Four a _ _ d) <> (Four a' b' c' d') = Four  (a <> a') b' c' (d' <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type ArbTwo = Two String [Int]
type ArbThree = Three String [Int] String
type ArbFour = Four String String String [Int]

testTuples :: IO ()
testTuples = do
  hspec $ do
    describe "Two" $ do
      prop "is semigroup associative" (semigroupAssoc :: ArbTwo -> ArbTwo -> ArbTwo -> Bool)
      prop "left identity" (monoidLeftIdentity :: ArbTwo -> Bool)
      prop "right identity" (monoidRightIdentity :: ArbTwo -> Bool)
    describe "Three" $ do
      prop "is semigroup associative" (semigroupAssoc :: ArbThree -> ArbThree -> ArbThree -> Bool)
    describe "Four" $ do
      prop "is semigroup associative" (semigroupAssoc :: ArbFour -> ArbFour -> ArbFour -> Bool)

--

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

testBoolies :: IO ()
testBoolies = do
  hspec $ do
    describe "Conjunction" $ do
      it "False <> False == False" $ do
        BoolConj False <> BoolConj False `shouldBe` BoolConj False
      it "False <> True == False" $ do
        BoolConj False <> BoolConj True `shouldBe` BoolConj False
      it "True <> False == False" $ do
        BoolConj True <> BoolConj False `shouldBe` BoolConj False
      it "True <> True == True" $ do
        BoolConj True <> BoolConj True `shouldBe` BoolConj True
      prop "semigroup is associative" (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
      prop "mempty is left identity" (monoidLeftIdentity :: BoolConj -> Bool)
      prop "mempty is right identity" (monoidRightIdentity :: BoolConj -> Bool)
    describe "Disjunction" $ do
      it "False <> False == False" $ do
        BoolDisj False <> BoolDisj False `shouldBe` BoolDisj False
      it "False <> True == True" $ do
        BoolDisj False <> BoolDisj True `shouldBe` BoolDisj True
      it "True <> False == True" $ do
        BoolDisj True <> BoolDisj False `shouldBe` BoolDisj True
      it "True <> True == True" $ do
        BoolDisj True <> BoolDisj True `shouldBe` BoolDisj True
      prop "semigroup is associative" (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
      prop "mempty is left identity" (monoidLeftIdentity :: BoolDisj -> Bool)
      prop "mempty is right identity" (monoidRightIdentity :: BoolDisj -> Bool)

--

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd a <> _ = Snd a
  _ <> Snd b = Snd b
  _ <> a = a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Fst a)), (1, return (Snd b))]

type OrInts = Or Int Int

testOr :: IO ()
testOr = do
  hspec $ do
    describe "Or semigroup" $ do
      it "Fst 1 <> Snd 2 == Snd 2" $ do Fst 1 <> Snd 2 `shouldBe` (Snd 2 :: OrInts)
      it "Fst 1 <> Fst 2 == Fst 2" $ do Fst 1 <> Fst 2 `shouldBe` (Fst 2 :: OrInts)
      it "Snd 1 <> Fst 2 == Snd 1" $ do Snd 1 <> Fst 2 `shouldBe` (Snd 1 :: OrInts)
      it "Snd 1 <> Snd 2 == Snd 1" $ do Snd 1 <> Snd 2 `shouldBe` (Snd 1 :: OrInts)
      prop "is associative" (semigroupAssoc :: OrInts -> OrInts -> OrInts -> Bool)


--

newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \a -> f a <> g a

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty

testCombine :: IO ()
testCombine = do
  hspec $ do
    describe "Combine semigroup" $ do
      let f = Combine $ \n -> Sum (n + 1)
      let g = Combine $ \n -> Sum (n - 1)
      it "unCombine (f <> g) 0 == 0" $ do unCombine (f <> g) 0 `shouldBe` Sum 0
      it "unCombine (f <> g) 1 == 2" $ do unCombine (f <> g) 1 `shouldBe` Sum 2
      it "unCombine (f <> f) 1 == 4" $ do unCombine (f <> f) 1 `shouldBe` Sum 4
      it "unCombine (g <> f) 1 == 2" $ do unCombine (g <> f) 1 `shouldBe` Sum 2
      prop "is associative" prop_combineSemigroupAssoc
      prop "has left identity" prop_combineLeftIdentity
      prop "has right identity" prop_combineRightIdentity

prop_combineSemigroupAssoc :: Sum Int -> Bool
prop_combineSemigroupAssoc x = unCombine (f <> (g <> h)) x == unCombine ((f <> g) <> h) x
  where f = Combine $ \n -> Sum (n + 1)
        g = Combine $ \n -> Sum (n - 1)
        h = Combine $ \n -> Sum (n * n)

prop_combineLeftIdentity :: Int -> Bool
prop_combineLeftIdentity x = unCombine (mappend f mempty) x == unCombine f x
  where f = Combine $ \n -> Sum (n + 1)

prop_combineRightIdentity :: Int -> Bool
prop_combineRightIdentity x = unCombine (mappend mempty f) x == unCombine f x
  where f = Combine $ \n -> Sum (n + 1)


--

newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ f . g

instance Monoid (Comp a) where
  mempty = Comp id

testComp :: IO ()
testComp = do
  hspec $ do
    describe "Compose semigroup" $ do
      let f = Comp $ \n -> n + 1
      let g = Comp $ \n -> n * 5
      it "unComp (f <> g) 0 == 1" $ do unComp (f <> g) 0 `shouldBe` 1
      it "unComp (g <> f) 0 == 5" $ do unComp (g <> f) 0 `shouldBe` 5
      it "unComp (f <> g) 1 == 6" $ do unComp (f <> g) 1 `shouldBe` 6
      it "unComp (g <> f) 1 == 10" $ do unComp (g <> f) 1 `shouldBe` 10
      it "unComp (f <> f) 1 == 3" $ do unComp (f <> f) 1 `shouldBe` 3
      prop "is associative" prop_composeSemigroupAssoc
      prop "has left identity" prop_composeLeftIdentity
      prop "has right identity" prop_composeRightIdentity

prop_composeSemigroupAssoc :: Sum Int -> Bool
prop_composeSemigroupAssoc x = unComp (f <> (g <> h)) x == unComp ((f <> g) <> h) x
  where f = Comp $ \n -> n + 1
        g = Comp $ \n -> n - 1
        h = Comp $ \n -> n * n

prop_composeLeftIdentity :: Int -> Bool
prop_composeLeftIdentity x = unComp (mappend f mempty) x == unComp f x
  where f = Comp $ \n -> n + 1

prop_composeRightIdentity :: Int -> Bool
prop_composeRightIdentity x = unComp (mappend mempty f) x == unComp f x
  where f = Comp $ \n -> n + 1


--

data Validation a b = Failur a | Succes b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failur x <> Failur y = Failur $ x <> y
  Failur _ <> x = x
  x <> Failur _ = x
  x <> _ = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Failur a)), (1, return (Succes b))]

type ValStrInts = Validation String Int

testFaildome :: IO ()
testFaildome = do
  let failure :: String -> Validation String Int
      failure = Failur
      success :: Int -> Validation String Int
      success = Succes
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2
  hspec $ do
    describe "Validation semigroup" $ do
      prop "is associative" (semigroupAssoc :: ValStrInts -> ValStrInts -> ValStrInts -> Bool)

--


newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
  -- Mem f1 <> Mem f2 = Mem $ \s -> (fst (f1 s) <> fst (f2 s), snd (f2 (snd (f1 s))))
  Mem f1 <> Mem f2 = Mem $ \s -> let (a1, s1) = f1 s
                                     (a2, s2) = f2 s1
                                 in (a1 <> a2, s2)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ (,) mempty

testMem :: IO ()
testMem = do
  hspec $ do
    describe "'Unit' tests" $ do
      let f' = Mem $ \s -> ("hi", s + 1)
          rmzero = runMem mempty 0
          rmleft = runMem (f' <> mempty) 0
          rmright = runMem (mempty <> f') 0
      it "rmleft" $ do rmleft `shouldBe` ("hi", 1)
      it "rmright" $ do rmright `shouldBe` ("hi", 1)
      it "rmzero" $ do (rmzero :: (String, Int)) `shouldBe` ("", 0)
      it "rmleft == runMem f' 0" $ do rmleft == runMem f' 0 `shouldBe` True
      it "rmright == runMem f' 0" $ do rmright == runMem f' 0 `shouldBe` True
    describe "quickcheck tests" $ do
      prop "Mem is associative" prop_memSemigroupAssoc
      prop "Mem has left identity" prop_memLeftIdentity
      prop "Mem has right identity" prop_memRightIdentity

prop_memSemigroupAssoc :: Int -> Bool
prop_memSemigroupAssoc n = runMem (f <> (g <> h)) n == runMem ((f <> g) <> h) n
  where
    f = Mem $ \s -> (replicate s 'a', s)
    g = Mem $ \s -> (take s "alabalaportocala", s + 1)
    h = Mem $ \s -> (drop s "howmuchwoodwouldawoodchuckchuckifawoodchuckcouldchuckwood", s * 3)

prop_memLeftIdentity :: Int -> Bool
prop_memLeftIdentity x = runMem (mappend f mempty) x == runMem f x
  where f = Mem $ \n -> (replicate n "hi", n + 1)

prop_memRightIdentity :: Int -> Bool
prop_memRightIdentity x = runMem (mappend mempty f) x == runMem f x
  where f = Mem $ \n -> (replicate n "hi", n + 1)
