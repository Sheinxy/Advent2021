module Main where

import Data.List
import Data.List.Split

type Line = [(Int, Int)]

allPointsNonDiag :: [[Int]] -> Line
allPointsNonDiag [[x1, y1], [x2, y2]]
  | x1 == x2 || y1 == y2 = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]
  | otherwise = []
allPointsNonDiag _ = error "Invalid line"

allPoints :: [[Int]] -> Line
allPoints [[x1, y1], [x2, y2]]
  | x1 == x2 || y1 == y2 = [(x, y) | x <- xs, y <- ys]
  | otherwise = zip xs ys
  where src = [min x1 x2 .. max x1 x2]
        dst = [min y1 y2 .. max y1 y2]
        xs = if x1 > x2 then reverse src else src
        ys = if y1 > y2 then reverse dst else dst
allPoints _ = error "Invalid line"

parseInput :: ([[Int]] -> Line) -> [String] -> [Line]
parseInput f = map $ f . map (map read . splitOn ",") . splitOn " -> "

parseInputNonDiag :: [String] -> [Line]
parseInputNonDiag = parseInput allPointsNonDiag

parseInputAll :: [String] -> [Line]
parseInputAll = parseInput allPoints

overlaps :: [Line] -> Int
overlaps = length . filter ((/=) 1 . length) . group . sort . concat

overlapsNonDiag :: [String] -> Int
overlapsNonDiag = overlaps . parseInputNonDiag

overlapsAll :: [String] -> Int
overlapsAll = overlaps . parseInputAll

main = do
  input <- lines <$> readFile "input"
  print $ overlapsNonDiag input
  print $ overlapsAll input

