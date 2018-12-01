{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace
import Data.Maybe
import Text.Read

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
