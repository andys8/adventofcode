{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day05
  ( main
  , test
  )
where

import Common.Test
import qualified Data.Map.Strict as Map
import qualified Data.List as List

main :: IO ()
main = do
  fileString <- readFile "src/input/07.txt"
  let fileLines = lines fileString
  print $ solvePartA fileLines
  -- print $ solvePartB fileLines
  return ()

-- Part A

data Order = Order Char Char deriving (Show, Eq)

solvePartA :: [String] -> String
solvePartA strLines = str
 where
  orders = parseLine <$> strLines
  str    = graphToString $ toGraph orders

parseLine :: String -> Order
parseLine str = Order from to
 where
  parts    = words str
  from : _ = parts !! 1
  to   : _ = parts !! 7

toFrom :: Order -> Char
toFrom (Order from _) = from

toTo :: Order -> Char
toTo (Order _ to) = to

allChars :: [Order] -> String
allChars orders = List.nub $ map toFrom orders ++ map toTo orders

toGraph :: [Order] -> Map.Map Char String
toGraph orders =
  let
    pending = foldl
      (\acc (Order from to) -> Map.insertWith (++) to [from] acc)
      Map.empty
      orders
    done = filter (`Map.notMember` pending) $ allChars orders
  in foldl (\acc x -> Map.insert x "" acc) pending done

graphToString :: Map.Map Char String -> String
graphToString graph = if null graph
  then []
  else c : graphToString (filter (/= c) <$> Map.delete c graph)
  where c = head $ Map.keys $ Map.filter null graph


-- Part B

-- solvePartB :: [String] -> Int
-- solvePartB _ = error "Todo"

-- Test

test :: IO ()
test = do
  testSolvePartA
  return ()


sample :: [String]
sample =
  [ "Step C must be finished before step A can begin."
  , "Step C must be finished before step F can begin."
  , "Step A must be finished before step B can begin."
  , "Step A must be finished before step D can begin."
  , "Step B must be finished before step E can begin."
  , "Step D must be finished before step E can begin."
  , "Step F must be finished before step E can begin."
  ]

testSolvePartA :: IO ()
testSolvePartA = runTests solvePartA [(sample, "CABDFE")]

