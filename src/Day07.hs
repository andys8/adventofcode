{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day05
  ( main
  , test
  )
where

import Common.Test
import qualified Data.Map as Map
import Data.List
import Control.Arrow
import Debug.Trace
import Data.Maybe


main :: IO ()
main = do
  fileString <- readFile "src/input/07.txt"
  let fileLines = lines fileString
  print $ solvePartA fileLines
  -- print $ solvePartB 10000 fileLines
  return ()

-- Part A

data Order = Order Char Char deriving (Show, Eq)

solvePartA :: [String] -> String
solvePartA strLines = str
 where
  tuples = parseLine <$> strLines
  str    = stringify2 tuples


parseLine :: String -> Order
parseLine str = Order from to
 where
  parts    = words str
  from : _ = parts !! 1
  to   : _ = parts !! 7

-- stringify str []                     = str
-- stringify ""  ((Order from to) : os) = stringify ([from, to]) os
-- stringify str (o@(Order from to) : os) =
--   case (elemIndex from str, elemIndex to $ traceShowId str) of
--     (Just i, Just j)
--       | i < j -> stringify str os
--       | otherwise -> traceShowId
--       $  stringify (delete from $ delete to str) (os ++ [o])
--     (_, Just i) -> stringify
--       (let (before, after) = splitAt i str in before ++ [from] ++ after)
--       os
--     (Just i, _) -> stringify
--       (let (before, after) = splitAt (i + 1) str in before ++ [to] ++ after)
--       os
--     (Nothing, Nothing) -> stringify str (os ++ [o])


stringify2 :: [Order] -> String
stringify2 os = sortBy sortFn  start
 where
  beforeList = orderToBefore <$> os
  start      = nub $ foldl addOrderToString "" os
  sortFn c1 c2 | elem (Order c1 c2) os = LT
  sortFn c1 c2 | elem (Order c2 c1) os = GT
  sortFn c1 c2                         = compare
    (fromMaybe 0 $ elemIndex c1 beforeList)
    (fromMaybe 0 $ elemIndex c2 beforeList)

orderToBefore :: Order -> Char
orderToBefore (Order before _) = before

addOrderToString :: String -> Order -> String
addOrderToString str (Order before after) =
  case (elemIndex before str, elemIndex after str) of
    (Just _ , Just _ ) -> str
    (Just _ , Nothing) -> str ++ [after]
    (Nothing, Just _ ) -> str ++ [before]
    (Nothing, Nothing) -> str ++ [before, after]


-- Part B

solvePartB :: [String] -> Int
solvePartB _ = 10

-- Test

test :: IO ()
test = do
  testSolvePartA
  -- testParseCoord
  -- testField
  -- testDistance
  -- testDistanceField
  -- testShortestDistance
  -- testSolvePartB
  -- testDistToAll
  return ()


sample :: [String]
sample =
  [ "Step C must be finished before step F can begin."
  , "Step A must be finished before step B can begin."
  , "Step A must be finished before step D can begin."
  , "Step B must be finished before step E can begin."
  , "Step D must be finished before step E can begin."
  , "Step F must be finished before step E can begin."
  ]

testSolvePartA :: IO ()
testSolvePartA = runTests solvePartA [(sample, "CABDFE")]

