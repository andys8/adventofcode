{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day08
  ( main
  , test
  )
where

import Common.Test

main :: IO ()
main = do
  fileString <- readFile "src/Year2018/input/08.txt"
  let entries = intList fileString
  print $ solvePartA entries
  print $ solvePartB entries
  return ()

intList :: String -> [Int]
intList str = do
  ws <- words str
  return (read ws)

-- Part A

data Node a = Node { children :: [Node a], metadata :: [a] } deriving (Show)

solvePartA :: [Int] -> Int
solvePartA nums = metadataSum $ toNode nums

metadataSum :: Num a => Node a -> a
metadataSum Node { metadata, children } =
  sum metadata + sum (metadataSum <$> children)

toNode :: [Int] -> Node Int
toNode = fst . parse

parse :: [Int] -> (Node Int, [Int])
parse (nChildren : nMetadata : is) =
  let
    (children, metadataAndRest) = parseRec nChildren is []
    (metadata, otherRest      ) = splitAt nMetadata metadataAndRest
  in (Node {children = children, metadata = metadata}, otherRest)

parse _ = error "Unexpected string length"

parseRec :: Int -> [Int] -> [Node Int] -> ([Node Int], [Int])
parseRec 0 list nodes = (nodes, list)
parseRec n list nodes = parseRec (n - 1) rest (nodes ++ [node])
  where (node, rest) = parse list

-- Part B

solvePartB :: [Int] -> Int
solvePartB nums = valueOfNode $ toNode nums

valueOfNode :: Node Int -> Int
valueOfNode Node { children, metadata } | null children = sum metadata
valueOfNode Node { children, metadata } =
  let
    isValidIndex i = i >= 0 && i < length children
    indices = filter isValidIndex $ map (subtract 1) metadata
    nodes   = (children !!) <$> indices
  in sum $ valueOfNode <$> nodes


-- Test

test :: IO ()
test = do
  testSolvePartA
  testSolvePartB
  testValueOfNode
  return ()


sample :: String
sample = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

testSolvePartA :: IO ()
testSolvePartA = runTests solvePartA [(intList sample, 138)]

testSolvePartB :: IO ()
testSolvePartB = runTests solvePartB [(intList sample, 66)]

testValueOfNode :: IO ()
testValueOfNode =
  runTests valueOfNode [(Node {children = [], metadata = [1, 2]}, 3)]
