{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day03 (main, test) where

import Debug.Trace
import Text.Read
import Common.Test
import qualified Data.Map as Map
import Data.Maybe as Maybe
import Data.Foldable
import Data.Monoid
import Data.List
import qualified Data.Text as T
import qualified Data.Set as Set


main :: IO ()
main = do
  fileString <- readFile "src/input/03.txt"
  let fileLines = lines fileString
  putStrLn $ show $ solvePartA fileLines
  putStrLn $ show $ solvePartB fileLines
  return ()

-- Part A

-- Solution:
-- 1. Parse lines to list of records ({claimId, x, y, width, height })
-- 2. Insert into map: x,y -> [claimId]
-- 3. Count values where list length > 1

data Claim = Claim { claimId :: Int, x :: Int, y :: Int, width :: Int, height :: Int } deriving (Show, Eq, Ord)

solvePartA :: [String] -> Int
solvePartA lines = overlappingClaimCount map
 where
  claims = toClaim <$> lines
  map    = claimsToMap claims

toClaim :: String -> Claim
toClaim s = Claim
  { claimId = toInt claimId
  , x       = toInt x
  , y       = toInt y
  , width   = toInt width
  , height  = toInt height
  }
 where
  [beforeAt, afterAt] = T.splitOn " @ " $ T.pack s
  claimId             = T.drop 1 beforeAt
  [position, size  ]  = T.splitOn ": " afterAt
  [x       , y     ]  = T.splitOn "," position
  [width   , height]  = T.splitOn "x" size
  toInt t = read (T.unpack t) :: Int

claimToMap :: Claim -> Map.Map String [Claim]
claimToMap claim@(Claim { x = posX, y = posY, width = width, height = height })
  = Map.fromList res
 where
  list =
    [ (x, y)
    | x <- [posX .. (posX + width - 1)]
    , y <- [posY .. (posY + height - 1)]
    ]
  res = fmap (\(x, y) -> (show x ++ "," ++ show y, [claim])) list

claimsToMap :: [Claim] -> Map.Map String [Claim]
claimsToMap claims = res
 where
  maps = fmap claimToMap claims
  res  = Map.unionsWith (++) maps



overlappingClaimCount :: Map.Map String [Claim] -> Int
overlappingClaimCount claimMap = length list3
 where
  list  = Map.toList claimMap
  list2 = snd <$> list
  list3 = filter ((> 1) . length) list2


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

-- Part B

solvePartB :: [String] -> Int
solvePartB lines = claimId claim
 where
  claims = toClaim <$> lines
  map    = claimsToMap claims
  overlapSet = overlappingClaims map
  totalSet = Set.fromList claims
  noOverlapSet = Set.difference totalSet overlapSet
  claim = head $ Set.toList noOverlapSet


overlappingClaims :: Map.Map String [Claim] -> Set.Set Claim
overlappingClaims claimMap = overlapping
 where
  list  = Map.toList claimMap
  list2 = snd <$> list
  list3 = filter ((> 1) . length) list2
  overlapping = Set.fromList $ concat list3


-- idMatch :: String -> String -> Bool
-- idMatch s1 s2 = length (toId s1 s2) == length s1 - 1

-- toId :: String -> String -> String
-- toId s1 s2 = map fst $ filter (uncurry (==)) $ zip s1 s2

-- Test

test = do
  testToClaim
  testClaimToMap
  testSolvePartA
  testSolvePartB

testToClaim = runTests
  toClaim
  [ ("#1 @ 1,3: 4x4", Claim {claimId = 1, x = 1, y = 3, width = 4, height = 4})
  , ("#3 @ 5,5: 1x2", Claim {claimId = 3, x = 5, y = 5, width = 1, height = 2})
  ]

testClaimToMap = runTests
  claimToMap
  [ let claim = Claim {claimId = 3, x = 2, y = 5, width = 2, height = 2}
    in  ( claim
        , Map.fromList
          [ ("2,5", [claim])
          , ("3,5", [claim])
          , ("2,6", [claim])
          , ("3,6", [claim])
          ]
        )
  ]

testSolvePartA =
  runTests solvePartA [(["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"], 4)]

testSolvePartB =
  runTests solvePartB [(["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"], 3)]



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
