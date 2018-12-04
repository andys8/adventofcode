{-# LANGUAGE OverloadedStrings #-}

module Day03
  ( main
  , test
  )
where

import Debug.Trace
import Common.Test
import Data.Map (Map)
import Data.Text (splitOn, unpack, pack)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as Map
import Data.Dates
import Data.Dates.Formats
import Data.Attoparsec.Text
import Data.Word
import Control.Applicative
import Data.Either
import Data.List
import Data.Function


main :: IO ()
main = do
  fileString <- readFile "src/input/04.txt"
  let fileLines = lines fileString
  print $ solvePartA fileLines
  print $ solvePartB fileLines
  return ()

-- Part A

data Log = Log LogType DateTime deriving (Show, Eq)
data LogType = ShiftStart Int | FallsAsleep | WakesUp deriving (Show, Eq, Ord)

instance Ord Log where
    (Log _ d1) `compare` (Log _ d2) = d1 `compare` d2

solvePartA :: [String] -> Int
solvePartA lines = guardId * minute
 where
  logs               = sort $ rights $ fmap toLog lines
  grouped            = groupLogs logs
  sleepMap           = Map.fromListWith (++) $ sleepyMinutes <$> grouped
  (guardId, minutes) = findGuardWithMostSleep $ Map.toList sleepMap
  minute             = head $ mostCommons minutes

findGuardWithMostSleep :: [(Int, [Int])] -> (Int, [Int])
findGuardWithMostSleep list = last $ sortOn (\(_, mins) -> length mins) list

mostCommons :: [Int] -> [Int]
mostCommons [] = []
mostCommons list = maximumBy (compare `on` length) $ group $ sort list

toLog :: String -> Either String Log
toLog str = parseOnly logParser $ pack str


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
  sleepAnalysis logs (mins ++ sleepToMinutes d1 d2)
sleepAnalysis (_ : rest) mins = sleepAnalysis rest mins

sleepToMinutes :: DateTime -> DateTime -> [Int]
sleepToMinutes d1 d2 = [m1 .. m2]
 where
  m1 = minute d1
  m2 = minute d2 - 1

timeParser :: Parser DateTime
timeParser = do
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
  return $ ShiftStart (fromIntegral guardId)

logTypeParser :: Parser LogType
logTypeParser =
  shiftStartParser
    <|> (string "falls asleep" >> return FallsAsleep)
    <|> (string "wakes up" >> return WakesUp)

logParser :: Parser Log
logParser = do
  t       <- timeParser
  _       <- char ' '
  logType <- logTypeParser
  return $ Log logType t


-- Part B

solvePartB :: [String] -> Int
solvePartB lines = guardId * minute
 where
  logs      = sort $ rights $ fmap toLog lines
  grouped   = groupLogs logs
  sleepMap  = Map.fromListWith (++) $ sleepyMinutes <$> grouped
  sleepList =  Map.toList sleepMap
  mapped = (\(i,mins) -> (i, mostCommons mins)) <$> sleepList
  res = sortOn (\(i, commons) -> length commons) $ traceShowId mapped
  (guardId, minute : _) = last $ traceShowId res
  -- (guardId, minutes) = findGuardWithMostSleep $ Map.toList sleepMap
  -- minute             = mostCommons minutes


-- Test

test :: IO ()
test = do
  testToLog
  testMostCommons
  testSolvePartA
  testSolvePartB

testToLog :: IO ()
testToLog = runTests
  toLog
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

testSolvePartA = runTests solvePartA [(sampleLog, 10 * 24)]

testSolvePartB = runTests solvePartB [(sampleLog, 99 * 45)]

testMostCommons = runTests mostCommons [([], [])]

-- testSolvePartB =
--   runTests solvePartB [(["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"], 3)]

