{-# LANGUAGE OverloadedStrings #-}

module Day02a (main, test) where

import Debug.Trace
import Text.Read
import Common.Test
import Data.Map.Strict as Map hiding (map, foldr')
import Data.Foldable
import Data.Monoid

main :: IO Integer
main = do
  fileString <- readFile "02/02.txt"
  return $ solvePartA $ lines fileString

solvePartA :: (Eq a,Num a) => [String] -> a
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

-- Test

test = do
  testCharCounts
  testCheckSum
  testTotalCheckSum
  testSolvePartA

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
