module Main where

import Data.List.Split

parseInput :: String -> [Int]
parseInput input = map (\x -> length . filter (== x) $ fishes) [0 .. 8]
    where fishes = map read . splitOn "," $ input

newDay :: [Int] -> [Int]
newDay [x0, x1, x2, x3, x4, x5, x6, x7, x8] = [x1, x2, x3, x4, x5, x6, x7 + x0, x8, x0]
newDay _ = error "Invalid school"

nDays :: Int -> [Int] -> Int
nDays n = sum . flip (!!) n . iterate newDay

main = do
  input <- parseInput <$> readFile "input"
  print $ nDays 80 input
  print $ nDays 256 input
