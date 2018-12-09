{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day08
  ( main
  , test
  )
where

import Common.Test
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Debug.Trace

main :: IO ()
main = do
  fileString <- readFile "src/input/08.txt"
  let entries = intList fileString
  print $ solvePartA entries
  -- print $ solvePartB fileLines
  return ()

intList :: String -> [Int]
intList str = read <$> words str

-- Part A

data Node a = Node { children :: [Node a], metadata :: [a] } deriving (Show)

solvePartA :: [Int] -> Int
solvePartA nums = metadataSum $ fst $ parseB nums

metadataSum :: Num a => Node a -> a
metadataSum (Node { metadata, children }) =
  sum metadata + sum (metadataSum <$> children)


parseB :: [Int] -> (Node Int, [Int])
parseB (nChildren : nMetadata : rest) =
  (Node {children = children, metadata = metadata}, rest3)
 where
  (children, rest2) = parseRec nChildren rest []
  (metadata, rest3) = splitAt nMetadata rest2

parseRec :: Int -> [Int] -> [Node Int] -> ([Node Int], [Int])
parseRec 0 list nodes = (nodes, list)
parseRec n list nodes =
  let (node, rest) = parseB list in parseRec (n-1) rest (nodes ++ [node])

-- Part B

-- solvePartB :: [String] -> Int
-- solvePartB _ = error "Todo"

-- Test

test :: IO ()
test = do
  testSolvePartA
  return ()


sample :: String
sample = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

testSolvePartA :: IO ()
testSolvePartA = runTests solvePartA [(intList sample, 138)]

