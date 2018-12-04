{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day03 (main, test) where

import Debug.Trace
import Common.Test
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (splitOn, unpack, pack)
import qualified Data.Text as T
import qualified Data.Set as S


main :: IO ()
main = do
  fileString <- readFile "src/input/03.txt"
  let fileLines = lines fileString
  putStrLn $ show $ solvePartA fileLines
  putStrLn $ show $ solvePartB fileLines
  return ()

-- Part A

data Claim = Claim { claimId, x, y, width, height :: Int } deriving (Show, Eq, Ord)
data Coord = Coord Int Int deriving (Show, Eq, Ord)

solvePartA :: [String] -> Int
solvePartA = length . overlaps . claimsToMap . fmap toClaim

toClaim :: String -> Claim
toClaim s = Claim
  { claimId = toInt claimId
  , x       = toInt x
  , y       = toInt y
  , width   = toInt width
  , height  = toInt height
  }
 where
  toInt t = read (unpack t) :: Int
  [beforeAt, afterAt] = splitOn " @ " $ pack s
  claimId             = T.drop 1 beforeAt
  [position, size  ]  = splitOn ": " afterAt
  [x       , y     ]  = splitOn "," position
  [width   , height]  = splitOn "x" size

claimToMap :: Claim -> Map Coord [Claim]
claimToMap claim@(Claim { x = posX, y = posY, width = width, height = height })
  = Map.fromList
    [ (Coord x y, [claim])
    | x <- [posX .. (posX + width - 1)]
    , y <- [posY .. (posY + height - 1)]
    ]

claimsToMap :: [Claim] -> Map Coord [Claim]
claimsToMap = Map.unionsWith (++) . fmap claimToMap

overlaps :: Map Coord [Claim] -> [[Claim]]
overlaps claimMap = filter ((> 1) . length) $ snd <$> Map.toList claimMap


-- Part B

solvePartB :: [String] -> Int
solvePartB lines =
  let claims = toClaim <$> lines
      diff a b = S.toList $ S.difference (S.fromList a) (S.fromList b)
  in  claimId $ head $ claims `diff` (concat $ overlaps $ claimsToMap claims)

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
          [ (Coord 2 5, [claim])
          , (Coord 3 5, [claim])
          , (Coord 2 6, [claim])
          , (Coord 3 6, [claim])
          ]
        )
  ]

testSolvePartA =
  runTests solvePartA [(["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"], 4)]

testSolvePartB =
  runTests solvePartB [(["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"], 3)]


