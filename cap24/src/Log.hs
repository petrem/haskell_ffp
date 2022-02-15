{-# LANGUAGE  QuasiQuotes #-}
module Log where

import Control.Applicative
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as Merge
import Data.Time
  ( Day
  , DiffTime
  , TimeOfDay
  , defaultTimeLocale
  , diffLocalTime
  , diffTimeToPicoseconds
  , formatTime
  , fromGregorian
  , makeTimeOfDayValid
  , midnight
  , timeOfDayToTime
  )
import Text.RawString.QQ
import Text.Trifecta

import Text.Integrals
import Text.Naturals


logSample :: String
logSample = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

logSample2 :: String
logSample2 = [r|
--growing apace with my Country

--  I give thanks to the Party!
-- Gosh! what bullshit.

# -99-01-01
05:00 Oh wow, hundred years to go
|]

logSample3 :: String
logSample3 = [r|
# 2022-02-14
06:30 Sad

# 2022-03-1
06:30 Sad

# 2022-3-8
06:30 Sad

# 2022-3-8
06:30 Sad

# 2022-12-24
06:30 Busy
19:00 Sad
|]

data Log = Log LogPreamble [LogEntry]
newtype LogPreamble = LogPreamble [Comment]
data LogEntry = LogEntry Day Comment [LogActivity]
data LogActivity = LogActivity TimeOfDay Activity Comment
type Activity = String
newtype Comment = Comment (Maybe String)

instance Show Log where
  show (Log pre es) = unlines $ show pre: map show es

instance Show LogPreamble where
  show (LogPreamble cs) = unlines $ map show cs

instance Show LogEntry where
  show (LogEntry day comment as) =
    unlines $ unwords [
      "#"
      , formatTime defaultTimeLocale "%F" day
      , show comment
      ] : map show as

instance Show LogActivity where
  show (LogActivity time activity comment) =
    unwords [ formatTime defaultTimeLocale "%R" time
            , activity
            , show comment
            ]

-- Using '//' instead of '--' to make easiery to debug it was parsed properly
-- and isn't just part of the log line
instance Show Comment where
  show (Comment (Just c)) = "// " ++ c
  show (Comment Nothing) = ""


parseLog :: Parser Log
parseLog = do
  preamble <- parsePreamble
  spaces
  entries <- many parseLogEntry
  eof
  return $ Log preamble entries

parsePreamble :: Parser LogPreamble
parsePreamble = LogPreamble <$> many (try (spaces >> Comment . Just <$> parseComment))

parseLogEntry :: Parser LogEntry
parseLogEntry = do
  (day, comment) <- parseEntryHeader
  activities <- many parseActivity
  return $ LogEntry day comment activities

parseEntryHeader :: Parser (Day, Comment)
parseEntryHeader = do
  spaces
  _ <- char '#'
  blanks
  date <- parseDate
  comment <- optional parseComment
  skipEOL
  return (date, Comment comment)

parseActivity :: Parser LogActivity
parseActivity = do
  time <- parseTime
  blanks
  activity <- nonComment
  comment <- optional parseComment
  skipEOL
  return $ LogActivity time activity (Comment comment)

-- Not exactly correct, since it parses month and days that aren't formed
-- of two digits. But meh.
parseDate :: Parser Day
parseDate = do
  year <- parseIntegral
  _ <- char '-'
  month <- checkBetween 1 12 "month" =<< parseNatural0'
  _ <- char '-'
  day <- checkBetween 1 31 "day" =<< parseNatural0'
  return $ fromGregorian year month day
  where
    checkBetween :: Int -> Int -> String -> Int -> Parser Int
    checkBetween low high valueName value =
      if value < low || value > high
      then fail ("Invalid " ++ valueName ++ ": " ++ show value)
      else return value

-- Not exactly correct, accepts non-two-digit things, blah blah.
parseTime :: Parser TimeOfDay
parseTime = do
  hh <- parseNatural0'
  _ <- char ':'
  mm <- parseNatural0'
  let maybeTime = makeTimeOfDayValid hh mm 0
  case maybeTime of
    (Just time) -> return time
    Nothing -> fail $ "Couldn't make time out of " ++ show hh ++ " and " ++ show mm

parseComment :: Parser String
parseComment =
  blanks *> string "--" *> blanks *>
  some (noneOf "\n") <* skipEOL

blanks :: Parser ()
blanks = skipMany (char ' ')

skipEOL :: Parser ()
skipEOL = skipMany newline

nonComment :: Parser String
nonComment =
  many
    (try (char '-' <* notFollowedBy (char '-'))
     <|> try (char ' ' <* notFollowedBy (string "--"))
     <|> noneOf "- \n"
    )

ps p = parseString p mempty


newtype DurationLog a = DL [DurationLogEntry a] deriving Show
data DurationLogEntry a = DLE Day [(Activity, a)] deriving Show


logToDL :: Num a => Log -> DurationLog a
logToDL (Log _ ls) = DL $ map logEntryToDLE ls

logEntryToDLE :: Num a => LogEntry -> DurationLogEntry a
logEntryToDLE (LogEntry d _ as) = DLE d $ zipWith activityDuration (drop 1 as) as

activityDuration :: Num a => LogActivity -> LogActivity -> (Activity, a)
activityDuration (LogActivity t1 _ _) (LogActivity t2 a _) =
  (a, fromInteger . diffTimeToPicoseconds $ timeOfDayToTime t1 - timeOfDayToTime t2)

-- aggregateDailyWith ::
--      Num a
--   => Merge.SimpleWhenMissing Activity a b
--   -> (a -> b -> b)
--   -> [DurationLogEntry a]
--   -> Map Activity b
-- aggregateDailyWith g f = foldr go M.empty
--   where
--     go :: a -> Map Activity b -> Map Activity b
--     go = Merge.merge g g (Merge.zipWithMatched $ const f) . dailyTimePerActivity


aggregateMapWith' :: Ord k => (a -> [a] -> [a]) -> [Map k a] -> Map k [a]
aggregateMapWith' f = foldr go M.empty
  where
    --go :: Map k a -> Map k b -> Map k b
    go = Merge.merge makeSingletonWhenMissing Merge.preserveMissing (Merge.zipWithMatched (const f))

aggregateMapWith ::
     Ord k => Merge.SimpleWhenMissing k a b -> (a -> b -> b) -> [Map k a] -> Map k b
aggregateMapWith missing_left agg = foldr go M.empty
  where
    --go :: Map k a -> Map k b -> Map k b
    go = Merge.merge missing_left Merge.preserveMissing (Merge.zipWithMatched (const agg))


dailyTimePerActivity :: Num a => DurationLogEntry a -> Map Activity a
dailyTimePerActivity (DLE _ as) = M.fromListWith (+) as


timePerActivity :: Num a => DurationLog a -> Map Activity a
timePerActivity (DL ls) = aggregateMapWith Merge.preserveMissing (+) (dailyTimePerActivity <$> ls)

avgTimePerActivity :: (Fractional a, Num a) => DurationLog a -> Map Activity a
avgTimePerActivity (DL ls) = avg <$> aggregateMapWith makeSingletonWhenMissing (:) (dailyTimePerActivity <$> ls)

makeSingletonWhenMissing :: Merge.SimpleWhenMissing k a [a]
makeSingletonWhenMissing = Merge.mapMissing (const pure)

avg :: (Fractional a, Num a) => [a] -> a
avg = (/) <$> sum <*> fromIntegral . length
