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
  input <- readFile "src/Year2019/input/01.txt"
  -- print $ solvePartA input
  -- print $ solvePartB input
  return ()

-- Part A

solvePartA :: String -> Int
solvePartA = undefined


-- Part B

solvePartB :: String -> Int
solvePartB = undefined

-- Test

test :: IO ()
test = testSolvePartA

testSolvePartA :: IO ()
testSolvePartA = runTests solvePartA []
