{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day03 (main, test) where

import Debug.Trace
import Common.Test
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Bifunctor (bimap)


main :: IO ()
main = do
  fileString <- readFile "src/input/03.txt"
  let fileLines = lines fileString
  putStrLn $ show $ solvePartA fileLines
  putStrLn $ show $ solvePartB fileLines
  return ()

-- Part A

data Claim = Claim
  { claimId :: Int
  , x       :: Int
  , y       :: Int
  , width   :: Int
  , height  :: Int
  }
 deriving (Show, Eq, Ord)

type PosClaimsMap = Map.Map String [Claim]

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
  toInt t = read (T.unpack t) :: Int
  [beforeAt, afterAt] = T.splitOn " @ " $ T.pack s
  claimId             = T.drop 1 beforeAt
  [position, size  ]  = T.splitOn ": " afterAt
  [x       , y     ]  = T.splitOn "," position
  [width   , height]  = T.splitOn "x" size

claimToMap :: Claim -> PosClaimsMap
claimToMap claim@(Claim { x = posX, y = posY, width = width, height = height })
  = Map.fromList
    [ (stringId x y, [claim])
    | x <- [posX .. (posX + width - 1)]
    , y <- [posY .. (posY + height - 1)]
    ]
  where stringId x y = show x ++ "," ++ show y

claimsToMap :: [Claim] -> PosClaimsMap
claimsToMap = Map.unionsWith (++) . fmap claimToMap

overlaps :: PosClaimsMap -> [[Claim]]
overlaps claimMap = filter ((> 1) . length) $ snd <$> Map.toList claimMap


-- Part B

solvePartB :: [String] -> Int
solvePartB lines = claimId $ head $ Set.toList overlapFree
 where
  claims      = toClaim <$> lines
  overlapFree = uncurry Set.difference $ bimap
    Set.fromList
    (Set.fromList . concat . overlaps . claimsToMap)
    (claims, claims)

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


