module Main where

import Data.List
import Data.List.Split
import Data.Matrix

parseInput :: String -> ([(Int, Int)], [(Int, Int)])
parseInput input = (points, folds)
  where inputLines = lines input 
        points = map ((\[x, y] -> (read x, read y)) . splitOn ",") . takeWhile (/= "") $ inputLines
        folds = map ((\[a, b] -> if last a == 'x' then (read b, 0) else (0, read b)) . splitOn "=") . tail . dropWhile (/= "") $ inputLines

foldOn :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
foldOn (xf, yf) = nub . map foldPaper
  where foldPaper (x, y) = (abs(xf - abs(xf - x)), abs(yf - abs(yf - y)))

allFolds :: ([(Int, Int)], [(Int, Int)]) -> [[(Int, Int)]]
allFolds (points, folds) = scanl (flip foldOn) points folds

buildMatrix :: [(Int, Int)] -> Matrix Char
buildMatrix points = matrix (maxY + 1) (maxX + 1) (\(y, x) -> if (x - 1, y - 1) `elem` points then '#' else '.')
  where maxX = foldl max 0 $ map fst points
        maxY = foldl max 0 $ map snd points

main = do
  input <- parseInput <$> readFile "input"
  print $ length $ allFolds input !! 1
  print $ buildMatrix . last . allFolds $ input
