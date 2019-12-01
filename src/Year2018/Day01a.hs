{-# LANGUAGE OverloadedStrings #-}

module Day01a (main, test) where

import Debug.Trace
import Data.Maybe
import Text.Read
import Common.Test

main = do
  fileString <- readFile "src/Year2018/input/01.txt"
  return $ solve fileString

solve :: String -> Integer
solve = sum . map toInt . lines

stripPlus :: String -> String
stripPlus ('+':a) = a
stripPlus a       = a

toInt :: String -> Integer
toInt s = read (stripPlus s) :: Integer


-- Test

test =
  runTests solve [("+1\n+1\n+1", 3), ("+1\n+1\n-2", 0), ("-1\n-2\n-3", -6)]

