{-# LANGUAGE OverloadedStrings #-}

module Day05
  ( main
  , test
  )
where

import Common.Test
import Data.Char


main = do
  input <- readFile "src/Year2019/input/01.txt"
  print $ solvePartA input
  print $ solvePartB input
  return ()

-- Part A

solvePartA :: String -> Int
solvePartA = sum . ((calculateFuel . read) <$>) . lines
  where calculateFuel mass = (mass `div` 3) - 2


-- Part B

solvePartB :: String -> Int
solvePartB = sum . ((calculateFuelRec 0 . floor . read) <$>) . lines
 where
  calculateFuelRec fuel mass =
    let fuel' = (mass `div` 3) - 2
    in if fuel' <= 0 then fuel else calculateFuelRec (fuel + fuel') fuel'

-- Test

test :: IO ()
test = runTests solvePartB [("14", 2), ("1969", 966), ("100756", 50346)]
