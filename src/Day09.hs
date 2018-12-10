{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day09
  ( main
  , test
  )
where

import Common.Test
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

main :: IO ()
main = do
  print $ solvePartA 458 72019
  -- print $ solvePartB entries
  return ()

-- Part A

data Marble = Marble Int deriving (Show, Eq, Ord)
data Player = Player Int deriving (Show, Eq, Ord)
type Circle = [Marble]
data State = State { marbles :: [Marble], circle :: Circle, players :: [Player], scores :: Map Player Int , lastMarblePoints :: Int} deriving (Show)


createPlayers :: Int -> [Player]
createPlayers n = Player <$> [1 .. n]

initialState :: Int -> State
initialState nPlayers =
  let
    marbles = Marble <$> [0 ..]
    players = createPlayers nPlayers
    scores  = Map.fromList [ (p, 0) | p <- players ]
  in State
    { circle           = [head marbles]
    , marbles          = tail marbles
    , players          = players
    , scores           = scores
    , lastMarblePoints = 0
    }

placeMarble :: Marble -> Circle -> Circle
placeMarble marble circle = marble : newCircle
  where newCircle = applyTimes 2 rotateLeft circle

removeMarble :: Circle -> (Marble, Circle)
removeMarble circle = (marble, newCircle)
  where marble : newCircle = applyTimes 7 rotateRight circle

-- anticlockwise
rotateLeft :: [a] -> [a]
rotateLeft [] = []
rotateLeft xs = tail xs ++ [head xs]

-- clockwise
rotateRight :: [a] -> [a]
rotateRight [] = []
rotateRight xs = last xs : init xs

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes n f x = iterate f x !! n

currentMarble :: Circle -> Marble
currentMarble = head

isSpecial :: Marble -> Bool
isSpecial (Marble n) = n `mod` 23 == 0

points :: Marble -> Int
points (Marble n) = n

gameStep :: State -> State
gameStep (State { marbles, circle, players, scores })
  = let
      marble    = head marbles
      player    = head players
      newCircle = if isSpecial marble
        then snd $ removeMarble circle
        else placeMarble marble circle
      worth = if isSpecial marble
        then
          let otherMarble = fst $ removeMarble circle
          in points marble + points otherMarble
        else 0
      newScores = if isSpecial marble
        then Map.insertWith (+) player worth scores
        else scores
    in State
      { marbles          = tail marbles
      , players          = rotateLeft players
      , circle           = newCircle
      , scores           = newScores
      , lastMarblePoints = points marble
      }


solvePartA :: Int -> Int -> Int
solvePartA nPlayers endingMarblePoints =
  let
    state = initialState nPlayers
    steps = iterate gameStep state
    isEndStep (State { lastMarblePoints }) = lastMarblePoints >= endingMarblePoints
    (State { scores }) = head $ dropWhile (not . isEndStep) steps
    highscore          = maximum $ snd <$> Map.toList scores
  in highscore


-- Part B

-- solvePartB :: [Int] -> Int
-- solvePartB nums = valueOfNode $ toNode nums


-- Test

test :: IO ()
test = do
  testSolvePartA
  testRotate
  testPlaceMarble
  testRemoveMarble
  -- testSolvePartB
  return ()



testSolvePartA :: IO ()
testSolvePartA = runTests
  (uncurry solvePartA)
  [ ((10, 1618), 8317)
  , ((13, 7999), 146373)
  , ((17, 1104), 2764)
  , ((21, 6111), 54718)
  , ((30, 5807), 37305)
  ]

testRotate = runTests
  id
  [(rotateRight [1, 2, 3], [3, 1, 2]), (rotateLeft [1, 2, 3], [2, 3, 1])]

testPlaceMarble = runTests
  (uncurry placeMarble)
  [ ((Marble 1, [Marble 0])          , [Marble 1, Marble 0])
  , ((Marble 2, [Marble 1, Marble 0]), [Marble 2, Marble 1, Marble 0])
  , ((Marble 3, Marble <$> [2, 1, 0]), Marble <$> [3, 0, 2, 1])
  ]

testRemoveMarble = runTests
  removeMarble
  [ ( Marble
      <$> [ 22
          , 11
          , 1
          , 12
          , 6
          , 13
          , 3
          , 14
          , 7
          , 15
          , 0
          , 16
          , 8
          , 17
          , 4
          , 18
          , 9
          , 19
          , 2
          , 20
          , 10
          , 21
          , 5
          ]
    , ( Marble 9
      , Marble
        <$> [ 19
            , 2
            , 20
            , 10
            , 21
            , 5
            , 22
            , 11
            , 1
            , 12
            , 6
            , 13
            , 3
            , 14
            , 7
            , 15
            , 0
            , 16
            , 8
            , 17
            , 4
            , 18
            ]
      )
    )
  ]
