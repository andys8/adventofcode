{-# LANGUAGE OverloadedStrings #-}

module Day05
  ( main
  , test
  )
where

import Common.Test
import qualified Data.Map as Map
import Data.List
import Control.Arrow
import Data.Maybe


main :: IO ()
main = do
  fileString <- readFile "src/input/06.txt"
  let fileLines = lines fileString
  print $ solvePartA fileLines
  print $ solvePartB 10000 fileLines
  return ()

-- Part A

data Coord = Coord Int Int deriving (Show, Eq, Ord)
data FieldSize = FieldSize Coord Coord deriving (Show, Eq)
data Distance = Distance Int deriving (Show, Eq, Ord)
data DistanceTo = DistanceTo Coord Distance deriving (Show, Eq)
data DistanceField = DistanceField [(Coord, DistanceTo)] deriving (Show, Eq)
data MergedDistanceField =
  MergedDistanceField [(Coord, [DistanceTo])] deriving (Show, Eq)
data ResultDistanceField =
  ResultDistanceField [(Coord, Maybe DistanceTo)] deriving (Show, Eq)

instance Ord DistanceTo where
  compare (DistanceTo _ d1) (DistanceTo _ d2) = compare d1 d2

solvePartA :: [String] -> Int
solvePartA fileLines =
  toMaxSize size $ resultDistanceField $ mergeDistanceFields distanceFields
 where
  coords         = parseCoord <$> fileLines
  size           = fieldSize coords
  distanceFields = distanceField size <$> coords

parseCoord :: String -> Coord
parseCoord str = Coord (read x) (read y)
  where [x, y] = words $ filter (/= ',') str

fieldSize :: [Coord] -> FieldSize
fieldSize coords = FieldSize
  (Coord (minimum xs) (minimum ys))
  (Coord (maximum xs) (maximum ys))
 where
  xs = (\(Coord x _) -> x) <$> coords
  ys = (\(Coord _ y) -> y) <$> coords

distanceField :: FieldSize -> Coord -> DistanceField
distanceField (FieldSize (Coord minX minY) (Coord maxX maxY)) coord =
  DistanceField allDistances
 where
  allCoords    = [ Coord x y | x <- [minX .. maxX], y <- [minY .. maxY] ]
  allDistances = (\c -> (c, DistanceTo coord $ distance c coord)) <$> allCoords

distance :: Coord -> Coord -> Distance
distance (Coord x1 y1) (Coord x2 y2) = Distance $ xDiff + yDiff
 where
  xDiff = abs $ x1 - x2
  yDiff = abs $ y1 - y2

toInt :: Distance -> Int
toInt (Distance i) = i

mergeDistanceFields :: [DistanceField] -> MergedDistanceField
mergeDistanceFields fields = MergedDistanceField $ merge mergables
 where
  wrapInList (DistanceField list) = mapSnd pure list
  mergables = fields >>= wrapInList
  merge     = Map.toList . Map.fromListWith (++)


resultDistanceField :: MergedDistanceField -> ResultDistanceField
resultDistanceField (MergedDistanceField list) =
  ResultDistanceField $ mapSnd shortestDistance list

shortestDistance :: [DistanceTo] -> Maybe DistanceTo
shortestDistance []  = Nothing
shortestDistance [d] = Just d
shortestDistance ds  = if d1 == d2 then Nothing else Just dist1
  where dist1@(DistanceTo _ d1) : DistanceTo _ d2 : _ = sort ds

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f = fmap (second f)

toMaxSize :: FieldSize -> ResultDistanceField -> Int
toMaxSize size resField@(ResultDistanceField list) = maximumSize
 where
  infinites = infiniteCoords size resField
  notInfinite (_, Just (DistanceTo c _)) = c `notElem` infinites
  notInfinite _                          = False
  _coord (DistanceTo c _) = c
  resCoords   = fmap _coord $ catMaybes $ snd <$> filter notInfinite list
  maximumSize = maximum $ fmap length $ group $ sort resCoords


infiniteCoords :: FieldSize -> ResultDistanceField -> [Coord]
infiniteCoords size (ResultDistanceField list) = nub $ mapMaybe isInf list
 where
  (FieldSize (Coord minX minY) (Coord maxX maxY)) = size
  isEdge (Coord x y) = x == minX || x == maxX || y == minY || y == maxY
  isInf (coord, Just (DistanceTo c _)) | isEdge coord = Just c
  isInf _ = Nothing


-- Part B

solvePartB :: Int -> [String] -> Int
solvePartB exclusiveLimit fileLines = length safeCoords
 where
  coords     = parseCoord <$> fileLines
  (FieldSize (Coord minX minY) (Coord maxX maxY)) = fieldSize coords
  allCoords  = [ Coord x y | x <- [minX .. maxX], y <- [minY .. maxY] ]
  safeCoords = filter (<exclusiveLimit)
    $ fmap (toInt . (`distToAll` coords)) allCoords


distToAll :: Coord -> [Coord] -> Distance
distToAll coord coords = Distance $ sum $ fmap (toInt . distance coord) coords

-- Test

test :: IO ()
test = do
  testSolvePartA
  testParseCoord
  testField
  testDistance
  testDistanceField
  testShortestDistance
  testSolvePartB
  testDistToAll
  return ()


sample :: [String]
sample = ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]

coordSample :: [Coord]
coordSample = parseCoord <$> sample

testSolvePartA :: IO ()
testSolvePartA = runTests solvePartA [(sample, 17)]

testParseCoord :: IO ()
testParseCoord =
  runTests parseCoord [("1, 2", Coord 1 2), ("12, 123", Coord 12 123)]

testField :: IO ()
testField =
  runTests fieldSize [(coordSample, FieldSize (Coord 1 1) (Coord 8 9))]

testDistance :: IO ()
testDistance = runTests
  (distance (Coord 3 4))
  [ (Coord 3 4, Distance 0)
  , (Coord 4 4, Distance 1)
  , (Coord 1 4, Distance 2)
  , (Coord 3 5, Distance 1)
  , (Coord 4 5, Distance 2)
  , (Coord 2 2, Distance 3)
  ]

testDistanceField :: IO ()
testDistanceField = runTests
  (uncurry distanceField)
  [ let coord = Coord 1 1
    in
      ( (FieldSize (Coord 1 1) (Coord 2 2), coord)
      , DistanceField
        [ (Coord 1 1, DistanceTo coord $ Distance 0)
        , (Coord 1 2, DistanceTo coord $ Distance 1)
        , (Coord 2 1, DistanceTo coord $ Distance 1)
        , (Coord 2 2, DistanceTo coord $ Distance 2)
        ]
      )
  ]

testShortestDistance :: IO ()
testShortestDistance
  = let
      coord1 = Coord 1 1
      coord2 = Coord 2 2
      coord3 = Coord 3 3
      dist1  = Distance 1
      dist2  = Distance 2
    in runTests
      shortestDistance
      [ ([DistanceTo coord1 dist1], Just $ DistanceTo coord1 dist1)
      , ([DistanceTo coord1 dist1, DistanceTo coord2 dist1], Nothing)
      , ( [ DistanceTo coord1 dist2
          , DistanceTo coord3 dist1
          , DistanceTo coord2 dist2
          ]
        , Just $ DistanceTo coord3 dist1
        )
      ]

testSolvePartB :: IO ()
testSolvePartB = runTests (solvePartB 32) [(sample, 16)]

testDistToAll :: IO ()
testDistToAll =
  runTests (uncurry distToAll) [((Coord 4 3, coordSample), Distance 30)]

