module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Lib


main :: IO ()
main = do
  hspec $ do
    describe "Identity" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: Identity String -> Bool)
      prop "Functor Composability" (prop_CheckComposability :: Identity Int -> Bool)
    describe "Pair" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: Pair String -> Bool)
      prop "Functor Composability" (prop_CheckComposability :: Pair Int -> Bool)
    describe "Two" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: Two String [Int] -> Bool)
      prop
        "Functor Composability"
        (prop_CheckComposability :: Two (Maybe Int) Int -> Bool)
    describe "Three" $ do
      prop
        "Functor Identity"
        (prop_FunctorIdentity :: Three String [Int] (Int, Int) -> Bool)
      prop
        "Functor Composability"
        (prop_CheckComposability :: Three (Maybe Int) Float Int -> Bool)
    describe "Three'" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: Three' [Int] (Int, Int) -> Bool)
      prop "Functor Composability" (prop_CheckComposability :: Three' Float Int -> Bool)
    describe "Four" $ do
      prop
        "Functor Identity"
        (prop_FunctorIdentity :: Four Float String [Int] (Int, Int) -> Bool)
      prop
        "Functor Composability"
        (prop_CheckComposability :: Four Int (Maybe Int) Float Int -> Bool)
    describe "Four'" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: Four' Int Integer -> Bool)
      prop
        "Functor Composability"
        (prop_CheckComposability :: Four' (Maybe Int) Int -> Bool)
    describe "Possibly" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: Possibly String -> Bool)
      prop "Functor Composability" (prop_CheckComposability :: Possibly Int -> Bool)
    describe "Sum" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: Sum [String] [Int] -> Bool)
      prop
        "Functor Composability"
        (prop_CheckComposability :: Sum (Maybe Int) Integer -> Bool)
    describe "Quant" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: Quant [String] [Int] -> Bool)
      prop
        "Functor Composability"
        (prop_CheckComposability :: Quant (Maybe Int) Integer -> Bool)
    describe "K" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: K Int (Identity Int) -> Bool)
      prop
        "Functor Composability"
        (prop_CheckComposability :: K (Maybe Int) Integer -> Bool)
    describe "Flip K'" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: Flip K' Int Int -> Bool)
      prop "Functor Composability" (prop_CheckComposability :: Flip K' Int Int -> Bool)
    describe "EvilGoateeConst" $ do
      prop
        "Functor Identity"
        (prop_FunctorIdentity :: EvilGoateeConst String Int -> Bool)
      prop
        "Functor Composability"
        (prop_CheckComposability :: EvilGoateeConst String Int -> Bool)
    describe "LiftItOut" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: LiftItOut Maybe Int -> Bool)
      prop
        "Functor Composability"
        (prop_CheckComposability :: LiftItOut Maybe Int -> Bool)
    describe "Parappa" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: Parappa Maybe [] Int -> Bool)
      prop
        "Functor Composability"
        (prop_CheckComposability :: Parappa Maybe (Either String) Int -> Bool)
    describe "IgnoreOne" $ do
      prop
        "Functor Identity"
        (prop_FunctorIdentity :: IgnoreOne Maybe [] Int String -> Bool)
      prop
        "Functor Composability"
        (prop_CheckComposability :: IgnoreOne Maybe (Either String) Int Int -> Bool)
    describe "Notorious" $ do
      prop
        "Functor Identity"
        (prop_FunctorIdentity :: Notorious Maybe Float Int String -> Bool)
      prop
        "Functor Composability"
        (prop_CheckComposability :: Notorious (Either String) String Int Int -> Bool)
    describe "List" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: List String -> Bool)
      prop "Functor Composability" (prop_CheckComposability :: List Int -> Bool)
    describe "List" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: List String -> Bool)
      prop "Functor Composability" (prop_CheckComposability :: List Int -> Bool)
    describe "GoatLord" $ do
      prop "Functor Identity" (prop_FunctorIdentity :: GoatLord String -> Bool)
      prop "Functor Composability" (prop_CheckComposability :: GoatLord Int -> Bool)
    describe "TalkToMe" $ do
      prop "Functor Identity" (prop_TalkToMeIdentity :: Int -> TalkToMe Int -> Bool)
      prop "Functor Composability" (prop_TalkToMeComposability :: Int -> TalkToMe Int -> Bool)


prop_FunctorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
prop_FunctorIdentity f = fmap id f == id f

prop_FunctorComposability ::
     (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
prop_FunctorComposability f g x = (fmap f . fmap g) x == fmap (f . g) x

--

prop_CheckComposability :: (Functor f, Eq (f a), Num a) =>  f a -> Bool
prop_CheckComposability = prop_FunctorComposability f g
  where f = (+1)
        g = (*3)

-- testing TalkToMe is trickier

prop_TalkToMeIdentity :: (Eq a, Show a) => a -> TalkToMe a -> Bool
prop_TalkToMeIdentity _ Halt = isHalt $ fmap id Halt
prop_TalkToMeIdentity _ p@(Print s a) = Just (s, a) == maybePrint (fmap id p)
prop_TalkToMeIdentity a r@(Read f) = let s = show a in Just (f s) == maybeApplyRead s (fmap id r)
-- I could drop the Arbitrary TalkToMe instance and generate all the three variants from a string and an ``a``

prop_TalkToMeComposability :: (Num a, Eq a, Show a) => a -> TalkToMe a -> Bool
prop_TalkToMeComposability a t =
  case t of
    Halt -> isHalt $ (fmap g . fmap h) Halt
    Print _ _ -> Just True == comparePrint
    Read f -> Just True == compareRead
  where
    g = (+ 1)
    h = (* 3)
    comparePrint = (==) <$> maybePrint ((fmap g . fmap h) t) <*> maybePrint (fmap (g . h) t)
    compareRead = let s = show a in
      (==) <$> maybeApplyRead s ((fmap g . fmap h) t) <*> maybeApplyRead s (fmap (g . h) t)
