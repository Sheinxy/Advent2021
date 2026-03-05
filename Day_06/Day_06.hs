module Main where

import Data.List.Split

parseInput :: String -> [Int]
parseInput input = map (\x -> length . filter (== x) $ fishes) [0 .. 8]
    where fishes = map read . splitOn "," $ input

newDay :: [Int] -> [Int]
newDay school = map (\(x, y) -> y + if x == 6 then head school else 0) shift 
  where shift = zip [0 .. ] $ tail school ++ [head school]

nDays :: Int -> [Int] -> Int
nDays n = sum . flip (!!) n . iterate newDay

main = do
  input <- parseInput <$> readFile "input"
  print $ nDays 80 input
  print $ nDays 256 input
