{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day03 (main, test) where

import Debug.Trace
import Text.Read
import Common.Test
import Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import Data.Foldable
import Data.Monoid
import Data.List
import qualified Data.Text (splitOn)


main :: IO ()
main = do
  fileString <- readFile "03/input.txt"
  let fileLines = lines fileString
  putStrLn $ show $ solvePartA fileLines
  -- putStrLn $ show $ solvePartB fileLines
  return ()

-- Part A

-- Solution:
-- 1. Parse lines to list of records ({id, x, y, width, height })
-- 2. Insert into map: x,y -> [id]
-- 3. Count values where list length > 1

data Claim = Claim { id :: Int, x :: Int, y :: Int, width :: Int, height :: Int } deriving (Show, Eq)

solvePartA :: [String] -> Int
-- solvePartA = totalCheckSum . map checkSum . map countChars
solvePartA _ = 0

-- "#1 @ 1,3: 4x4"
toClaim :: String -> Maybe Claim
-- toClaim ('#':id:' ':'@':' ':x:',':y:": ":width:'x':height:[]) = Just (Claim {id = read id :: Int})
toClaim _ = Nothing


toClaim s = Just
  (Claim {id = id, x = x, y = y, width = width, height = height})
 where
  [beforeAt, afterAt] = splitOn " @ " s
  id                  = read (drop 1 beforeAt) :: Int
  [position, size  ]  = splitOn ": " afterAt
  [x       , y     ]  = splitOn "," position
  [width   , height]  = splitOn "x" size



-- countChars :: Num a => String -> Map.Map Char (a)
-- countChars str = foldr' (\c map -> Map.insertWith (+) c 1 map) Map.empty str

-- checkSum :: (Eq a, Num a) => Map.Map Char a -> (Bool, Bool)
-- checkSum charCounts = (hasN 2, hasN 3)
--  where
--   charCountList = Map.toList charCounts
--   findCount count = find ((==) count . snd)
--   hasN n = isJust $ findCount n charCountList

-- totalCheckSum :: [(Bool, Bool)] -> Int
-- totalCheckSum tupleList = (toSum fst) * (toSum snd)
--   where toSum focus = sum $ map (fromEnum . focus) tupleList

-- -- Part B

-- solvePartB :: [String] -> Maybe String
-- solvePartB ids = extractId $ findPair $ groupById ids
--  where
--   groupById = groupBy idMatch . sort
--   findPair  = find ((==) 2 . length)
--   extractId = fmap (\[a, b] -> toId a b)

-- idMatch :: String -> String -> Bool
-- idMatch s1 s2 = length (toId s1 s2) == length s1 - 1

-- toId :: String -> String -> String
-- toId s1 s2 = map fst $ filter (uncurry (==)) $ zip s1 s2

-- Test

test = do
  testToClaim
  -- testCharCounts
  -- testCheckSum
  -- testTotalCheckSum
  testSolvePartA
  -- testIdMatch
  -- testSolvePartB

testToClaim = runTests
  toClaim
  [ ( "#1 @ 1,3: 4x4"
    , Just (Claim {id = 1, x = 1, y = 3, width = 4, height = 4})
    )
  , ( "#3 @ 5,5: 1x2"
    , Just (Claim {id = 3, x = 5, y = 5, width = 1, height = 2})
    )
  ]

testSolvePartA =
  runTests solvePartA [(["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"], 4)]


-- testCharCounts = runTests
--   countChars
--   [ ("abc"   , Map.fromList [('a', 1), ('b', 1), ('c', 1)])
--   , ("aabbbc", Map.fromList [('a', 2), ('b', 3), ('c', 1)])
--   ]

-- testCheckSum = runTests
--   checkSum
--   [ (Map.fromList [('a', 1), ('b', 1), ('c', 1)], (False, False))
--   , (Map.fromList [('a', 2), ('b', 3), ('c', 1)], (True, True))
--   , (Map.fromList [('a', 2), ('b', 3), ('c', 2)], (True, True))
--   ]

-- testTotalCheckSum = runTests
--   totalCheckSum
--   [([(True, True), (True, False), (False, False)], 2 * 1)]

-- testIdMatch = runTests (idMatch "abcde")
--                        [("abcdf", True), ("safd", False), ("abcxx", False)]

-- testSolvePartB = runTests
--   solvePartB
--   [ ( ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
--     , Just "fgij"
--     )
--   ]
