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
  -- print $ solvePartA fileLines
  print $ solvePartB 10000 fileLines
  return ()

-- Part A

data Coord = Coord Int Int deriving (Show, Eq, Ord)
data FieldSize = FieldSize Coord Coord deriving (Show, Eq)
data Distance = Distance Int deriving (Show, Eq, Ord)
data DistanceTo = DistanceTo Coord Distance deriving (Show, Eq)
data DistanceField = DistanceField [(Coord, DistanceTo)] deriving (Show, Eq)
data MergedDistanceField = MergedDistanceField [(Coord, [DistanceTo])]
  deriving (Show, Eq)
data ResultDistanceField = ResultDistanceField [(Coord, Maybe DistanceTo)]
  deriving (Show, Eq)



instance Ord DistanceTo where
  compare (DistanceTo _ d1) (DistanceTo _ d2) = compare d1 d2

solvePartA :: [String] -> Int
solvePartA lines = maxSize
 where
  coords         = parseCoord <$> lines
  size           = fieldSize coords
  distanceFields = fmap (distanceField size) coords
  m              = mergeDistanceFields distanceFields
  r              = resultDistanceField m
  maxSize        = toMaxSize size r

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

mergeDistanceFields :: [DistanceField] -> MergedDistanceField
mergeDistanceFields fields = MergedDistanceField res
 where
  toMergable (DistanceField list) = mapSnd pure list
  mergables = fields >>= toMergable
  res       = Map.toList $ Map.fromListWith (++) mergables


resultDistanceField :: MergedDistanceField -> ResultDistanceField
resultDistanceField (MergedDistanceField list) =
  ResultDistanceField $ mapSnd shortestDistance list

shortestDistance :: [DistanceTo] -> Maybe DistanceTo
shortestDistance []       = Nothing
shortestDistance (d : []) = Just d
shortestDistance ds       = if d1 == d2 then Nothing else Just first
  where first@(DistanceTo _ d1) : (DistanceTo _ d2) : _ = sort ds

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f = fmap (\(a, b) -> (a, f b))

toMaxSize :: FieldSize -> ResultDistanceField -> Int
toMaxSize (FieldSize (Coord minX minY) (Coord maxX maxY)) (ResultDistanceField list)
  = maximumSize
 where
  isEdge (Coord x y) = x == minX || x == maxX || y == minY || y == maxY
  shouldBeDropped (coord, Just (DistanceTo c d)) =
    if isEdge coord then Just c else Nothing
  shouldBeDropped _ = Nothing
  infinites = nub $ catMaybes $ map shouldBeDropped list
  isGood (coord, Just (DistanceTo c d)) = notElem c infinites
  isGood (coord, _                    ) = False
  res = filter (isGood) list
  toCoord (DistanceTo c d) = c
  resCoords   = fmap toCoord $ catMaybes $ snd <$> res
  maximumSize = maximum $ fmap length $ group $ sort resCoords




-- Part B

solvePartB :: Int -> [String] -> Int
solvePartB exclusiveLimit lines = res
 where
  coords    = parseCoord <$> lines
  (FieldSize (Coord minX minY) (Coord maxX maxY)) = fieldSize coords
  allCoords = [ Coord x y | x <- [minX .. maxX], y <- [minY .. maxY] ]
  safeCoords =
    filter (\c -> toInt (distToAll c coords) < exclusiveLimit) allCoords
  res = length safeCoords


distToAll :: Coord -> [Coord] -> Distance
distToAll coord coords = Distance $ sum $ fmap (toInt . distance coord) coords

toInt (Distance i) = i

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

coordSample = parseCoord <$> sample

testSolvePartA :: IO ()
testSolvePartA = runTests solvePartA [(sample, 17)]

testParseCoord :: IO ()
testParseCoord =
  runTests parseCoord [("1, 2", Coord 1 2), ("12, 123", Coord 12 123)]

testField =
  runTests fieldSize [(coordSample, FieldSize (Coord 1 1) (Coord 8 9))]

testDistance = runTests
  (distance (Coord 3 4))
  [ (Coord 3 4, Distance 0)
  , (Coord 4 4, Distance 1)
  , (Coord 1 4, Distance 2)
  , (Coord 3 5, Distance 1)
  , (Coord 4 5, Distance 2)
  , (Coord 2 2, Distance 3)
  ]


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


testDistToAll =
  runTests (uncurry distToAll) [((Coord 4 3, coordSample), Distance 30)]

