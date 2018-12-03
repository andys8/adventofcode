{-# LANGUAGE OverloadedStrings #-}

module Day01b (main, test) where

import Debug.Trace
import Text.Read
import Data.Foldable
import Data.Maybe
import Common.Test


main :: IO ()
main = do
  fileString <- readFile "src/input/01.txt"
  putStrLn $ show $ solve fileString
  where solve = findResult . map toInt . lines

findResult :: (Eq a, Num a) => [a] -> Maybe a
findResult intList = findAcc [0] (cycle intList)
 where
  findAcc sums [] = Nothing
  findAcc sums (i:is) =
    let s = i + head sums
    in  if elem s sums then Just s else findAcc (s : sums) is

stripPlus :: String -> String
stripPlus ('+':a) = a
stripPlus a       = a

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

