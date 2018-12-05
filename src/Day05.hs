{-# LANGUAGE OverloadedStrings #-}

module Day05
  ( main
  , test
  )
where

import Common.Test
import Data.Char


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
  reactAcc as []       = as
  reactAcc []  (b : bs) = reactAcc [b] bs
  reactAcc (a : as) (b : bs)
    | reactPair a b = reactAcc as bs
    | otherwise     = reactAcc (b : a : as) bs

reactPair :: Char -> Char -> Bool
reactPair a b = a /= b && toLower a == toLower b

-- Part B

solvePartB :: String -> Int
solvePartB polymer =
  minimum $ (length . reactString . ($ polymer) . removeUnit) <$> ['a' .. 'z']

removeUnit :: Char -> String -> String
removeUnit c = filter ((/= toLower c) . toLower)

-- Test

test :: IO ()
test = do
  testSolvePartA
  testReactPair
  testReactString
  testSolvePartB
  testRemoveUnit

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

testRemoveUnit :: IO ()
testRemoveUnit = runTests
  (uncurry removeUnit)
  [(('a', "aBAac"), "Bc"), (('z', "asdf"), "asdf")]

testSolvePartB :: IO ()
testSolvePartB = runTests solvePartB [("dabAcCaCBAcCcaDA", 4)]

