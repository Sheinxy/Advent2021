module Main where

import Data.List
import Data.List.Split

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

median :: [Int] -> Int
median xs
  | even len = (sorted !! mid + sorted !! (mid - 1)) `div` 2
  | otherwise = sorted !! mid
  where len = length xs
        mid = len `div` 2
        sorted = sort xs

mean :: [Int] -> Int
mean xs = sum xs `div` length xs

fuelTo :: (Int -> Int -> Int) -> Int -> [Int] -> Int
fuelTo cost x = sum . map (cost x)

fuel :: (([Int] -> Int), ([Int] -> Int)) -> (Int -> Int -> Int) -> [Int] -> Int
fuel (start, end) cost xs = minimum [fuelTo cost x xs | x <- [s .. e]]
  where s = start xs
        e = end xs

main = do
  input <- parseInput <$> readFile "input"
  print $ fuel medians simpleCost input
  print $ fuel means quadraticCost input
  where simpleCost x xn = abs (x - xn)
        quadraticCost x xn = sum [1 .. abs (x - xn)]
        medians = (median, median)
        means = ((+) (-1) . mean, (+) 1 . mean)
