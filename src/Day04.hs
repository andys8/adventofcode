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
import Data.Attoparsec.Text
-- import Data.Attoparsec.Char8
import Data.Word
import Control.Applicative

main :: IO ()
main = do
  fileString <- readFile "src/input/04.txt"
  let fileLines = lines fileString
  print $ solvePartA fileLines
  print $ parseOnly timeParser "2013-06-30 14:33:29"
  -- print $ solvePartB fileLines
  return ()

-- Part A

data Claim = Claim { claimId, x, y, width, height :: Int } deriving (Show, Eq, Ord)
data Coord = Coord Int Int deriving (Show, Eq, Ord)

data Log = Log LogType DateTime deriving (Show, Eq, Ord)
data LogType = ShiftStart Integer | FallsAsleep | WakesUp deriving (Show, Eq, Ord)

solvePartA :: [String] -> String
solvePartA _ = "wrong"
-- solvePartA = length . overlaps . claimsToMap . fmap toClaim


toLog :: String -> Log
toLog str = res2
 where
  res  = parseOnly logParser $ pack str
  res2 = case res of
    Right t -> t
    Left  _ -> Log WakesUp (DateTime 2019 10 10 23 0 0)
  -- parts = traceShowId $ words str
  -- fixedDateString =
  --   filter (\c -> c /= '[' && c /= ']') $ traceShowId $ head parts
  -- tmp = traceShowId $ parseDateFormat "YYYY-MM-DD HH:mm" $ traceShowId
  --   fixedDateString
  -- res = case tmp of
  --   Left  _ -> Log ShiftStart (DateTime 2019 10 10 23 0 0)
  --   Right t -> Log ShiftStart t


timeParser :: Parser DateTime
timeParser = do
  _    <- char '['
  yyyy <- count 4 digit
  _    <- char '-'
  mm   <- count 2 digit
  _    <- char '-'
  dd    <- count 2 digit
  _    <- char ' '
  hh    <- count 2 digit
  _    <- char ':'
  m    <- count 2 digit
  _    <- char ']'
  return $ DateTime (read yyyy) (read mm) (read dd) (read hh) (read m) 0

parseShiftStart :: Parser LogType
parseShiftStart = do
  _ <- string "Guard #"
  guardId <- decimal
  return $ ShiftStart guardId

productParser :: Parser LogType
productParser =
  parseShiftStart
    <|> (string "falls asleep" >> return FallsAsleep)
    <|> (string "wakes up" >> return WakesUp)

logParser :: Parser Log
logParser = do
  t <- timeParser
  _ <- char ' '
  logType <- productParser
  return $ Log logType t

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
    , Log (ShiftStart 10) (DateTime 1518 11 01 0 0 0)
    )
  , ( "[1518-11-01 00:59] Guard #123 begins shift"
    , Log (ShiftStart 123) (DateTime 1518 11 01 0 59 0)
    )
  , ("[1518-11-04 00:46] wakes up", Log WakesUp (DateTime 1518 11 4 0 46 0))
  , ( "[1518-11-04 00:46] falls asleep"
    , Log FallsAsleep (DateTime 1518 11 4 0 46 0)
    )
  ]

testSolvePartA = runTests
  solvePartA
  [(["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"], "20")]

-- testSolvePartB =
--   runTests solvePartB [(["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"], 3)]

