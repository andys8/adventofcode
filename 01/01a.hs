{-# LANGUAGE OverloadedStrings #-}

module Day01a (main, test) where

import Debug.Trace
import Data.Maybe
import Text.Read
import Common.Test

stripPlus :: String -> String
stripPlus ('+':a) = a
stripPlus str     = str

toIntList :: [String] -> [Integer]
toIntList = map ((\l -> read l :: Integer) . stripPlus)

solve :: String -> Integer
solve = sum . toIntList . lines

main = do
  fileString <- readFile "01a.txt"
  return $ solve fileString

-- Test

test =
  runTests solve [("+1\n+1\n+1", 3), ("+1\n+1\n-2", 0), ("-1\n-2\n-3", -6)]

