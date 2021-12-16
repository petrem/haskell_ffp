module Phone where

import Control.Monad
import Data.Bool
import Data.Char
import Data.List
import Data.Maybe(maybe)

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

type Button = (Digit, [Char])

newtype DaPhone = DaPhone [Button]

newtype StringLength = SL {unSL :: String} deriving (Eq, Show)

instance Ord StringLength where
  (SL s1) <= (SL s2) = length s1 <= length s2

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol OK. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "OK. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

stdPhone :: DaPhone
stdPhone = DaPhone [ ('2', "abc2")
                   , ('3', "def3")
                   , ('4', "ghi4")
                   , ('5', "jkl5")
                   , ('6', "mno6")
                   , ('7', "pqr7")
                   , ('8', "stu8")
                   , ('9', "vxyz9")
                   , ('0', " 0")
                   , ('*', "^")
                   , ('#', ".,")
                   ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps layout c = bool [] (tryButtons layout '^') (isUpper c) ++ tryButtons layout (toLower c)
--reverseTaps (DaPhone layout) c = fmap ((+1) <$>) <$> (find (isJust . snd) $ map (fmap (elemIndex c)) layout)

-- assuming the default phone definition
-- 'a' -> [(2, 1)]
-- 'A' -> [('*', 1), ('2', 1)]


tryButton :: Char -> Button -> [(Digit, Presses)]
tryButton c (d, cs) = maybe [] (\i -> [(d, i + 1)]) (elemIndex c cs)

tryButtons :: DaPhone -> Char -> [(Digit, Presses)]
tryButtons (DaPhone buttons) c = concatMap (tryButton c) buttons

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = concatMap . reverseTaps

tapConvo :: [String] -> [[(Digit, Presses)]]
tapConvo = map (cellPhonesDead stdPhone)

--How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0


-- What is the most popular letter for each message? What was its cost?
mostPopularLetter :: String -> (Char, Presses)
mostPopularLetter = ap (,) (fingerTaps . reverseTaps stdPhone) . head . unSL . maximum . map SL . group . sort


-- What is the most popular letter overall? What is the overall most popular word?
coolestLtr :: [String] -> (Char, Presses)
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = head . maximumBy (\a b -> compare (length a) (length b)) . group .sort . map (filter isAlphaNum . map toLower) . concatMap words
