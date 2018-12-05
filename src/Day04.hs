{-# LANGUAGE OverloadedStrings #-}

module Day04
  ( main
  , test
  )
where

import Common.Test
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Dates
import Data.Attoparsec.Text
import Control.Applicative
import Data.Either
import Data.List
import Data.Function
import Control.Arrow


main :: IO ()
main = do
  fileString <- readFile "src/input/04.txt"
  let fileLines = lines fileString
  print $ solvePartA fileLines
  print $ solvePartB fileLines
  return ()

-- Part A

data Log = Log LogType DateTime deriving (Show, Eq)
data LogType = ShiftStart Int
             | FallsAsleep
             | WakesUp deriving (Show, Eq, Ord)

instance Ord Log where
    (Log _ d1) `compare` (Log _ d2) = d1 `compare` d2

solvePartA :: [String] -> Int
solvePartA logLines = guardId * commonMinute
 where
  sleepMap = Map.fromListWith (++) $ sleepyMinutes <$> groupLines logLines
  (guardId, minutes) = findGuardWithMostSleep $ Map.toList sleepMap
  commonMinute = head $ mostCommons minutes

groupLines :: [String] -> [[Log]]
groupLines logLines = groupLogs $ sort $ rights $ fmap parseLog logLines

findGuardWithMostSleep :: [(Int, [Int])] -> (Int, [Int])
findGuardWithMostSleep list = last $ sortOn (\(_, mins) -> length mins) list

mostCommons :: [Int] -> [Int]
mostCommons []   = []
mostCommons list = maximumBy (compare `on` length) $ group $ sort list

parseLog :: String -> Either String Log
parseLog str = parseOnly logParser $ T.pack str

groupLogs :: [Log] -> [[Log]]
groupLogs = groupBy groupFn
 where
  groupFn _ (Log (ShiftStart _) _) = False
  groupFn _ _                      = True

sleepyMinutes :: [Log] -> (Int, [Int])
sleepyMinutes logs = (guardId, minutes)
 where
  Log (ShiftStart guardId) _ : rest = logs
  minutes                           = sleepAnalysis rest []

sleepAnalysis :: [Log] -> [Int] -> [Int]
sleepAnalysis [] mins = mins
sleepAnalysis (Log FallsAsleep d1 : Log WakesUp d2 : logs) mins =
  sleepAnalysis logs (mins ++ minsBetweeenTimes d1 d2)
sleepAnalysis (_ : rest) mins = sleepAnalysis rest mins

minsBetweeenTimes :: DateTime -> DateTime -> [Int]
minsBetweeenTimes d1 d2 = [m1 .. m2]
 where
  m1 = minute d1
  m2 = minute d2 - 1

dateTimeParser :: Parser DateTime
dateTimeParser = do
  _    <- char '['
  yyyy <- count 4 digit
  _    <- char '-'
  mm   <- count 2 digit
  _    <- char '-'
  dd   <- count 2 digit
  _    <- char ' '
  hh   <- count 2 digit
  _    <- char ':'
  m    <- count 2 digit
  _    <- char ']'
  return $ DateTime (read yyyy) (read mm) (read dd) (read hh) (read m) 0


shiftStartParser :: Parser LogType
shiftStartParser = do
  _       <- string "Guard #"
  guardId <- decimal
  return $ ShiftStart guardId

logTypeParser :: Parser LogType
logTypeParser =
  shiftStartParser
    <|> (string "falls asleep" >> return FallsAsleep)
    <|> (string "wakes up" >> return WakesUp)

logParser :: Parser Log
logParser = do
  dateTime <- dateTimeParser
  _        <- char ' '
  logType  <- logTypeParser
  return $ Log logType dateTime


-- Part B

solvePartB :: [String] -> Int
solvePartB logLines = guardId * commonMinute
 where
  sleepMap = Map.fromListWith (++) $ sleepyMinutes <$> groupLines logLines
  mapped = Control.Arrow.second mostCommons <$> Map.toList sleepMap
  (guardId, commonMinute : _) = last $ sortOn (length . snd) mapped


-- Test

test :: IO ()
test = do
  testParseLog
  testMostCommons
  testSolvePartA
  testSolvePartB

testParseLog :: IO ()
testParseLog = runTests
  parseLog
  [ ( "[1518-11-01 00:00] Guard #10 begins shift"
    , Right $ Log (ShiftStart 10) (DateTime 1518 11 01 0 0 0)
    )
  , ( "[1518-11-01 00:59] Guard #123 begins shift"
    , Right $ Log (ShiftStart 123) (DateTime 1518 11 01 0 59 0)
    )
  , ( "[1518-11-04 00:46] wakes up"
    , Right $ Log WakesUp (DateTime 1518 11 4 0 46 0)
    )
  , ( "[1518-11-04 00:46] falls asleep"
    , Right $ Log FallsAsleep (DateTime 1518 11 4 0 46 0)
    )
  ]

sampleLog :: [String]
sampleLog =
  [ "[1518-11-01 00:00] Guard #10 begins shift"
  , "[1518-11-01 00:05] falls asleep"
  , "[1518-11-01 00:25] wakes up"
  , "[1518-11-01 00:30] falls asleep"
  , "[1518-11-01 00:55] wakes up"
  , "[1518-11-01 23:58] Guard #99 begins shift"
  , "[1518-11-02 00:40] falls asleep"
  , "[1518-11-02 00:50] wakes up"
  , "[1518-11-03 00:05] Guard #10 begins shift"
  , "[1518-11-03 00:24] falls asleep"
  , "[1518-11-03 00:29] wakes up"
  , "[1518-11-04 00:02] Guard #99 begins shift"
  , "[1518-11-04 00:36] falls asleep"
  , "[1518-11-04 00:46] wakes up"
  , "[1518-11-05 00:03] Guard #99 begins shift"
  , "[1518-11-05 00:45] falls asleep"
  , "[1518-11-05 00:55] wakes up"
  ]

testSolvePartA :: IO ()
testSolvePartA = runTests solvePartA [(sampleLog, 10 * 24)]

testSolvePartB :: IO ()
testSolvePartB = runTests solvePartB [(sampleLog, 99 * 45)]

testMostCommons :: IO ()
testMostCommons = runTests mostCommons [([], [])]

