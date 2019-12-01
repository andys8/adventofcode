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
import Data.Sequence as Seq

main :: IO ()
main = do
  print $ solvePartA 458 72019
  print $ solvePartA 458 (72019 * 100)
  return ()

-- Part A

newtype Marble = Marble Int deriving (Show, Eq, Ord)
newtype Player = Player Int deriving (Show, Eq, Ord)
type Circle = Seq Marble
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
    { circle           = Seq.singleton $ head marbles
    , marbles          = tail marbles
    , players          = players
    , scores           = scores
    , lastMarblePoints = 0
    }

placeMarble :: Marble -> Circle -> Circle
placeMarble marble circle = marble <| newCircle
  where newCircle = rotateLeftSeq2 circle

removeMarble :: Circle -> (Marble, Circle)
removeMarble circle = (Seq.index newCircle 0, Seq.drop 1 newCircle)
  where newCircle = rotateRightSeq7 circle

-- anticlockwise
rotateLeft :: [a] -> [a]
rotateLeft []       = []
rotateLeft (x : xs) = xs ++ [x]

rotateLeftSeq2 :: Seq a -> Seq a
rotateLeftSeq2 s | Seq.null s = s
rotateLeftSeq2 s              = b >< a where (a, b) = Seq.splitAt 2 s


-- clockwise
rotateRight :: [a] -> [a]
rotateRight [] = []
rotateRight xs = last xs : init xs

rotateRightSeq7 :: Seq a -> Seq a
rotateRightSeq7 s | Seq.null s = s
rotateRightSeq7 s = b >< a where (a, b) = Seq.splitAt (Seq.length s - 7) s


applyTimes :: Int -> (a -> a) -> a -> a
-- applyTimes n f x = iterate f x !! n
applyTimes 0 _ x = x
applyTimes n f x = applyTimes (n - 1) f (f x)

isSpecial :: Marble -> Bool
isSpecial (Marble n) = n `mod` 23 == 0

points :: Marble -> Int
points (Marble n) = n

gameStep :: State -> State
gameStep State { marbles, circle, players, scores } =
  let
    marble                 = head marbles
    player                 = head players
    (newCircle, newScores) = if isSpecial marble
      then
        let
          (takenMarble, circleWithoutMarble) = removeMarble circle
          pointsForPlayer = points marble + points takenMarble
          updatedScore = Map.insertWith (+) player pointsForPlayer scores
        in (circleWithoutMarble, updatedScore)
      else (placeMarble marble circle, scores)
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
    state            = initialState nPlayers
    State { scores } = applyTimes endingMarblePoints gameStep state
    highscore        = maximum $ snd <$> Map.toList scores
  in highscore


-- Test

test :: IO ()
test = do
  testSolvePartA
  testRotate
  testPlaceMarble
  testRemoveMarble
  testApplyNTimes
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

testRotate :: IO ()
testRotate = runTests
  id
  [(rotateRight [1, 2, 3], [3, 1, 2]), (rotateLeft [1, 2, 3], [2, 3, 1])]

testPlaceMarble :: IO ()
testPlaceMarble = runTests
  (uncurry placeMarble)
  [ ((Marble 1, Seq.fromList [Marble 0]), Seq.fromList [Marble 1, Marble 0])
  , ( (Marble 2, Seq.fromList [Marble 1, Marble 0])
    , Seq.fromList [Marble 2, Marble 1, Marble 0]
    )
  , ( (Marble 3, Marble <$> Seq.fromList [2, 1, 0])
    , Marble <$> Seq.fromList [3, 0, 2, 1]
    )
  ]

testApplyNTimes :: IO ()
testApplyNTimes = runTests id [(applyTimes 3 (subtract 1) 10, 7)]

testRemoveMarble :: IO ()
testRemoveMarble = runTests
  removeMarble
  [ ( Marble <$> Seq.fromList
      [ 22
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
      , Marble <$> Seq.fromList
        [ 19
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
