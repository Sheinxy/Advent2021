module Main where

import Data.Char
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

type Graph = Map String [String]

addEdge :: Graph -> (String, String) -> Graph
addEdge g (src, dst) = g'
  where withSrc = if not $ Map.member src g then Map.insert src [] g else g
        withDst = if not $ Map.member dst withSrc then Map.insert dst [] withSrc else withSrc
        withEdge1  = Map.adjust (\x -> if dst `elem` x then x else dst : x) src withDst
        g' = Map.adjust (\x -> if src `elem` x then x else src : x) dst withEdge1

parseInput :: String -> Graph
parseInput = foldl addEdge Map.empty . map getEdge . lines
  where getEdge s = (head $ splitOn "-" s, splitOn "-" s !! 1)

countPaths1 :: Graph -> Int
countPaths1 g = traverseFrom g marker 0 "start"
  where marker = Map.fromList $ zip (Map.keys g) (repeat False)
        traverseFrom _ _ acc "end" = acc + 1
        traverseFrom g m acc s = foldl (traverseFrom g m') acc adj
          where adj = filter (not . (m Map.!)) $ g Map.! s
                m' = if all isLower s then Map.insert s True m else m

countPaths2 :: Graph -> Int
countPaths2 g = traverseFrom g marker False 0 "start"
  where marker = Map.fromList $ zip (Map.keys g) (repeat False)
        traverseFrom _ _ _ acc "end" = acc + 1
        traverseFrom g m small acc s = foldl (traverseFrom g m' small') acc adj
          where small' = small || m Map.! s
                adj = filter (\x -> (not small' && x /= "start") || not (m Map.! x)) $ g Map.! s
                m' = if all isLower s then Map.insert s True m else m
main = do
  input <- parseInput <$> readFile "input"
  print $ countPaths1 input
  print $ countPaths2 input
