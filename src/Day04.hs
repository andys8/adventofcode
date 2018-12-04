{-# LANGUAGE OverloadedStrings #-}

module Day03
  ( main
  , test
  )
where

import Debug.Trace
import Common.Test
import Data.Map (Map)
import Data.Text (splitOn, unpack, pack)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as Map
import Data.Dates
import Data.Dates.Formats

main :: IO ()
main = do
  fileString <- readFile "src/input/04.txt"
  let fileLines = lines fileString
  print $ solvePartA fileLines
  -- print $ solvePartB fileLines
  return ()

-- Part A

data Claim = Claim { claimId, x, y, width, height :: Int } deriving (Show, Eq, Ord)
data Coord = Coord Int Int deriving (Show, Eq, Ord)

data Log = ShiftStart DateTime String deriving (Show, Eq, Ord)

solvePartA :: [String] -> String
solvePartA _ = "wrong"
-- solvePartA = length . overlaps . claimsToMap . fmap toClaim


toLog :: String -> Log
toLog str = res
 where
  parts = traceShowId $ words str
  fixedDateString =
    filter (\c -> c /= '[' && c /= ']') $ traceShowId $ head parts
  tmp = traceShowId $ parseDateFormat "YYYY-MM-DD HH:mm" $ traceShowId
    fixedDateString
  res = case tmp of
    Left  _ -> ShiftStart (DateTime 2019 10 10 23 0 0) "id"
    Right t -> ShiftStart t "id"




-- toClaim :: String -> Claim
-- toClaim s = Claim
--   { claimId = toInt claimId
--   , x       = toInt x
--   , y       = toInt y
--   , width   = toInt width
--   , height  = toInt height
--   }
--  where
--   toInt t = read (unpack t) :: Int
--   [beforeAt, afterAt] = splitOn " @ " $ pack s
--   claimId             = T.drop 1 beforeAt
--   [position, size  ]  = splitOn ": " afterAt
--   [x       , y     ]  = splitOn "," position
--   [width   , height]  = splitOn "x" size

-- claimToMap :: Claim -> Map Coord [Claim]
-- claimToMap claim@Claim { x = posX, y = posY, width = width, height = height } =
--   Map.fromList
--     [ (Coord x y, [claim])
--     | x <- [posX .. (posX + width - 1)]
--     , y <- [posY .. (posY + height - 1)]
--     ]

-- claimsToMap :: [Claim] -> Map Coord [Claim]
-- claimsToMap = Map.unionsWith (++) . fmap claimToMap

-- overlaps :: Map Coord [Claim] -> [[Claim]]
-- overlaps claimMap = filter ((> 1) . length) $ snd <$> Map.toList claimMap


-- Part B

-- solvePartB :: [String] -> Int
-- solvePartB lines =
--   let
--     claims = toClaim <$> lines
--     diff a b = S.toList $ S.difference (S.fromList a) (S.fromList b)
--   in claimId $ head $ claims `diff` concat (overlaps $ claimsToMap claims)

-- Test

test :: IO ()
test = do
  testToLog
  testSolvePartA
   -- testSolvePartB

testToLog :: IO ()
testToLog = runTests
  toLog
  [ ( "[1518-11-01 00:00] Guard #10 begins shift"
    , ShiftStart (DateTime 1518 11 01 0 0 0) "10"
    )
  , ( "[1518-11-01 00:59] Guard #123 begins shift"
    , ShiftStart (DateTime 1518 11 01 0 59 0) "123"
    )
  ]

testSolvePartA = runTests
  solvePartA
  [(["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"], "20")]

-- testSolvePartB =
--   runTests solvePartB [(["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"], 3)]

