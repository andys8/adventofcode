{-# LANGUAGE OverloadedStrings #-}

module Day02a (main, test) where

import Debug.Trace
import Text.Read
import Common.Test
import Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import Data.Foldable
import Data.Monoid
import Data.List

main :: IO ()
main = do
  fileString <- readFile "02/02.txt"
  let fileLines = lines fileString
  putStrLn $ show $ solvePartA fileLines
  putStrLn $ show $ solvePartB fileLines
  return ()

-- Part A

solvePartA :: (Eq a, Num a) => [String] -> a
solvePartA = totalCheckSum . map checkSum . map countChars

countChars :: Num a => String -> Map.Map Char (a)
countChars str = foldr' (\c map -> Map.insertWith (+) c 1 map) Map.empty str

checkSum :: (Eq a, Num a) => Map.Map Char a -> (a, a)
checkSum charCounts = (asValue twos, asValue threes)
 where
  charCountList = Map.toList charCounts
  twos          = find (\(_, count) -> count == 2) charCountList
  threes        = find (\(_, count) -> count == 3) charCountList
  asValue Nothing = 0
  asValue _       = 1

totalCheckSum :: Num a => [(a, a)] -> a
totalCheckSum tupleList = twos * threes
 where
  twos   = sum $ map (fst) tupleList
  threes = sum $ map (snd) tupleList

-- Part B

solvePartB ids = res2
 where
  sorted  = sort ids
  grouped = groupBy idMatch sorted
  res     = find (\l -> length l == 2) grouped
  res2    = fmap (\[a, b] -> toId (a, b)) res

idMatch str1 str2 = length filtered == 1
 where
  zipList  = zip str1 str2
  filtered = filter (\(a, b) -> a /= b) zipList

toId (str1, str2) = map fst $ filter (\(a, b) -> a == b) $ zip str1 str2

-- Test

test = do
  testCharCounts
  testCheckSum
  testTotalCheckSum
  testSolvePartA
  testIdMatch
  testSolvePartB

testCharCounts = runTests
  countChars
  [ ("abc"   , Map.fromList [('a', 1), ('b', 1), ('c', 1)])
  , ("aabbbc", Map.fromList [('a', 2), ('b', 3), ('c', 1)])
  ]

testCheckSum = runTests
  checkSum
  [ (Map.fromList [('a', 1), ('b', 1), ('c', 1)], (0, 0))
  , (Map.fromList [('a', 2), ('b', 3), ('c', 1)], (1, 1))
  , (Map.fromList [('a', 2), ('b', 3), ('c', 2)], (1, 1))
  ]

testTotalCheckSum = runTests totalCheckSum [([(1, 1), (1, 0), (0, 0)], 2 * 1)]

testSolvePartA =
  runTests solvePartA [(["aabbcc", "abb", "", "bbb", "aaabbb", "abbba"], 9)]

testIdMatch = runTests (idMatch "abcde")
                       [("abcdf", True), ("safd", False), ("abcxx", False)]

testSolvePartB = runTests
  solvePartB
  [ ( ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
    , Just "fgij"
    )
  ]
