import Data.List (sort)
import Test.Hspec
import Test.Hspec.QuickCheck -- for prop
import Test.QuickCheck

import Quicks


main :: IO ()
main = do
  sample trivialGen
  sample (identityGen :: Gen (Identity Bool))
  testIdentities

testIdentities :: IO ()
testIdentities = hspec $ do
  describe "half" $ do
    it "half identity of Ints" $ do
      quickCheck prop_halfIdentityInt
    it "half identity of Floats" $ do
      quickCheck prop_halfIdentityFloat
  describe "sort" $ do
    it "sorts any list of Ints" $ do
      quickCheck prop_listOrderedInt
  describe "addition" $ do
    it "commutative for Int" $ do
      quickCheck (plusCommutative :: Int -> Int -> Bool)
    it "commutative for Float" $ do
      quickCheck (plusCommutative :: Float -> Float -> Bool)
    it "associative for Int" $ do
      quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
    it "associative for Float" $ do
      quickCheck (plusAssociative :: Float -> Float -> Float -> Bool)
  describe "multiplication" $ do
    it "commutative for Int" $ do
      quickCheck (mulCommutative :: Int -> Int -> Bool)
    it "commutative for Float" $ do
      quickCheck (mulCommutative :: Float -> Float -> Bool)
    it "associative for Int" $ do
      quickCheck (mulAssociative :: Int -> Int -> Int -> Bool)
    it "associative for Float" $ do
      quickCheck (mulAssociative :: Float -> Float -> Float -> Bool)
  describe "generic operator checks" $ do
    it "addition commutative for Ints" $ do
      quickCheck (prop_commutative (+) :: Int -> Int -> Bool)
    it "multiplication associative for Double" $ do
      quickCheck (prop_associative (*) :: Double -> Double -> Double -> Bool)
    it "multiplication distributive over addition for Integer" $ do
      quickCheck (prop_distributive (*) (+) :: Integer -> Integer -> Integer -> Bool)
    it "addition distributive over multiplication for Integer" $ do
      quickCheck (prop_distributive (+) (*) :: Integer -> Integer -> Integer -> Bool)
  describe "integral division properties" $ do
    it "div and mod" $ property
      (prop_integerDivision div mod :: Integer -> NonZero Integer -> Bool)
    it "quot and rem" $ property
      (prop_integerDivision quot rem :: Integer -> NonZero Integer -> Bool)
    it "not: div and rem" $ property
      (prop_integerDivision div rem :: Integer -> NonZero Integer -> Bool)
    it "not: quot and mod" $ property
      (prop_integerDivision quot mod :: Integer -> NonZero Integer -> Bool)
  describe "the ^ operator" $ do
    it "is associative?" $ property
      (prop_associativeIntegerPower :: Int -> NonNegative Int -> NonNegative Int -> Bool)
    it "is commutative?" $ property
      (prop_commutativeIntegerPower :: NonNegative Int -> NonNegative Int -> Bool)
    it "is distributive over addition?" $ property
      (prop_distributiveIntegerPower (+) :: Int -> Int -> NonNegative Int -> Bool)
    it "is distributive over multiplication?" $ property
      (prop_distributiveIntegerPower (*) :: Int -> Int -> NonNegative Int -> Bool)
  describe "self inverse" $ do
    prop "reverse" (prop_selfInverse reverse :: String -> Bool)
  describe "$" $ do
    prop "works" (prop_apply (\x -> (x::Int) + 1))
  describe "foldr as append" $ do
    prop "works"  (prop_foldrConsVsAppend :: [Int] -> [Int] -> Bool)
  describe "foldr as concat" $ do
    prop "works"  (prop_foldrAppendVsConcat :: [[Int]] -> Bool)
  describe "weird length" $ do
    prop "equals n" prop_weird_length
  describe "show and read" $ do
    prop "show is read's inverse, using String" (prop_inverse show read :: String -> Bool)
    prop "show is read's inverse, using Int" (prop_inverse show read :: Int -> Bool)
  describe "square" $ do
    prop "sqrt should be inverse of squre, no?" (prop_inverse sqrt square :: Double -> Bool)

prop_halfIdentity :: (Eq a, Floating a) => a -> Bool
prop_halfIdentity x = x == halfIdentity x
  where halfIdentity = (2 *) . half

prop_halfIdentityInt :: Int -> Bool
prop_halfIdentityInt = prop_halfIdentity . fromIntegral

prop_halfIdentityFloat :: Float -> Bool
prop_halfIdentityFloat = prop_halfIdentity

-- for any list you apply sort to,
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

prop_listOrdered :: Ord a => [a] -> Bool
prop_listOrdered l = listOrdered (sort l)

prop_listOrderedInt :: [Int] -> Bool
prop_listOrderedInt = prop_listOrdered

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y =
  x + y == y + x

mulAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
mulAssociative x y z =
  x * (y * z) == (x * y) * z

mulCommutative :: (Eq a, Num a) => a -> a -> Bool
mulCommutative x y =
  x * y == y * x

type BinaryOperator a = a -> a -> a

prop_associative :: Eq a => BinaryOperator a -> a -> a -> a -> Bool
prop_associative f x y z =
  f x (f y z) == f (f x y) z

prop_commutative :: Eq a => BinaryOperator a -> a -> a -> Bool
prop_commutative f x y =
  f x y == f y x

prop_distributive :: Eq a => BinaryOperator a -> BinaryOperator a -> a -> a -> a -> Bool
prop_distributive f g x y z =
  f (g x y) z == g (f x z) (f y z)

prop_associativeIntegerPower :: (Integral a, Eq a) =>  a -> NonNegative a -> NonNegative a -> Bool
prop_associativeIntegerPower x (NonNegative y) (NonNegative z) =
  x ^ (y ^ z) == (x ^ y) ^ z

prop_commutativeIntegerPower :: (Integral a, Eq a) => NonNegative a -> NonNegative a -> Bool
prop_commutativeIntegerPower (NonNegative x) (NonNegative y) =
  x ^ y == y ^ x

prop_distributiveIntegerPower :: (Eq a, Integral a) => BinaryOperator a -> a -> a -> NonNegative a -> Bool
prop_distributiveIntegerPower op x y (NonNegative z) = prop_distributive (^) op x y z

prop_integerDivision :: (Eq a, Integral a) => BinaryOperator a -> BinaryOperator a -> a -> NonZero a -> Bool
prop_integerDivision f g x (NonZero y) = (f x y) * y + (g x y) == x

prop_selfInverse :: (Eq a) => (a -> a) -> a -> Bool
prop_selfInverse f = prop_inverse f f

prop_inverse :: (Eq a) => (a -> b) -> (b -> a) ->  a -> Bool
prop_inverse f g x = (g . f) x == x

prop_apply :: (Eq b) => (a -> b) -> a -> Bool
prop_apply f x = (f $ x) == f x

prop_foldrConsVsAppend :: Eq a => [a] -> [a] ->  Bool
prop_foldrConsVsAppend xs ys = foldr (:) xs ys == xs ++ ys

prop_foldrAppendVsConcat :: Eq a => [[a]] ->  Bool
prop_foldrAppendVsConcat xs = foldr (++) [] xs == concat xs

prop_weird_length :: Int -> [Int] -> Bool
prop_weird_length n xs = f n xs == n
  where f n' xs' = length (take n' xs')
