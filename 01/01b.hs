{-# LANGUAGE OverloadedStrings #-}

module Day01b (main, test) where

import Debug.Trace
import Text.Read
import Data.Foldable
import Data.Maybe
import Common.Test


findResult intList = findResultHelp [0] infiniteList
  where infiniteList = cycle intList


findResultHelp sums [] = Nothing
findResultHelp sums (i:is) =
  let currentSum = i + head sums
  in  if elem currentSum sums
        then Just currentSum
        else findResultHelp (currentSum : sums) is


main = do
  fileString <- readFile "01b.txt"
  return $ solve fileString
  where solve = findResult . map toInt . lines

stripPlus :: String -> String
stripPlus ('+':a) = a
stripPlus str     = str

toInt :: String -> Integer
toInt s = read (stripPlus s) :: Integer


-- Test

test = runTests
  findResult
  [ ([1, -1]           , Just 0)
  , ([3, 3, 4, -2, -4] , Just 10)
  , ([-6, 3, 8, 5, -6] , Just 5)
  , ([7, 7, -2, -7, -4], Just 14)
  ]

