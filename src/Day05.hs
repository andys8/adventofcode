{-# LANGUAGE OverloadedStrings #-}

module Day05
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
import Data.Char
import Debug.Trace


main :: IO ()
main = do
  fileString <- readFile "src/input/05.txt"
  let polymer = filter (/= '\n') fileString
  print $ solvePartA polymer
  print $ solvePartB polymer
  return ()

-- Part A

solvePartA :: String -> Int
solvePartA = length . reactString

reactString :: String -> String
reactString = reverse . reactAcc []
 where
  reactAcc acc []       = acc
  reactAcc []  (b : bs) = reactAcc [b] bs
  reactAcc (a : as) (b : bs)
    | reactPair a b = reactAcc as bs
    | otherwise     = reactAcc (b : a : as) bs

reactPair :: Char -> Char -> Bool
reactPair a b = a /= b && toLower a == toLower b

-- Part B

solvePartB :: String -> Int
solvePartB _ = 10


-- Test

test :: IO ()
test = do
  -- testParseLog
  -- testMostCommons
  testSolvePartA
  testReactPair
  testReactString
  -- testSolvePartB

testSolvePartA :: IO ()
testSolvePartA = runTests solvePartA [("dabAcCaCBAcCcaDA", 10)]

testReactString :: IO ()
testReactString = runTests reactString [("dabAcCaCBAcCcaDA", "dabCBAcaDA")]



testReactPair :: IO ()
testReactPair = runTests
  (uncurry reactPair)
  [ (('a', 'a'), False)
  , (('a', 'A'), True)
  , (('Z', 'z'), True)
  , (('a', 'B'), False)
  , (('A', 'A'), False)
  ]
-- testParseLog :: IO ()
-- testParseLog = runTests
--   parseLog
--   [ ( "[1518-11-01 00:00] Guard #10 begins shift"
--     , Right $ Log (ShiftStart 10) (DateTime 1518 11 01 0 0 0)
--     )
--   , ( "[1518-11-01 00:59] Guard #123 begins shift"
--     , Right $ Log (ShiftStart 123) (DateTime 1518 11 01 0 59 0)
--     )
--   , ( "[1518-11-04 00:46] wakes up"
--     , Right $ Log WakesUp (DateTime 1518 11 4 0 46 0)
--     )
--   , ( "[1518-11-04 00:46] falls asleep"
--     , Right $ Log FallsAsleep (DateTime 1518 11 4 0 46 0)
--     )
--   ]
-- testSolvePartB :: IO ()
-- testSolvePartB = runTests solvePartB [(sampleLog, 99 * 45)]

