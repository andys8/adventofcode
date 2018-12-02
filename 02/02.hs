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

solvePartA :: [String] -> Int
solvePartA = totalCheckSum . map checkSum . map countChars

countChars :: Num a => String -> Map.Map Char (a)
countChars str = foldr' (\c map -> Map.insertWith (+) c 1 map) Map.empty str

checkSum :: (Eq a, Num a) => Map.Map Char a -> (Bool, Bool)
checkSum charCounts = (hasN 2, hasN 3)
 where
  charCountList = Map.toList charCounts
  findCount count = find ((==) count . snd)
  hasN n = isJust $ findCount n charCountList

totalCheckSum :: [(Bool, Bool)] -> Int
totalCheckSum tupleList = (toSum fst) * (toSum snd)
  where toSum focus = sum $ map (fromEnum . focus) tupleList

-- Part B

solvePartB :: [String] -> Maybe String
solvePartB ids = extractId $ findPair $ groupById ids
 where
  groupById = groupBy idMatch . sort
  findPair  = find (\l -> length l == 2)
  extractId = fmap (\[a, b] -> toId a b)

idMatch :: String -> String -> Bool
idMatch s1 s2 = length id == length s1 - 1
    where id = toId s1 s2

toId :: String -> String -> String
toId s1 s2 = map fst $ filter (uncurry (==)) $ zip s1 s2

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
  [ (Map.fromList [('a', 1), ('b', 1), ('c', 1)], (False, False))
  , (Map.fromList [('a', 2), ('b', 3), ('c', 1)], (True, True))
  , (Map.fromList [('a', 2), ('b', 3), ('c', 2)], (True, True))
  ]

testTotalCheckSum = runTests
  totalCheckSum
  [([(True, True), (True, False), (False, False)], 2 * 1)]

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
