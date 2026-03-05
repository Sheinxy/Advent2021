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
mean xs = fromIntegral $ round $ s / l
  where s = fromIntegral $ sum xs
        l = fromIntegral $ length xs

fuelTo :: (Int -> Int -> Int) -> Int -> [Int] -> Int
fuelTo cost x = sum . map (cost x)

fuel :: (([Int] -> Int), ([Int] -> Int)) -> (Int -> Int -> Int) -> [Int] -> Int
fuel (start, end) cost xs = minimum [fuelTo cost x sorted | x <- [s .. e]]
  where sorted = sort xs
        s = start xs
        e = end xs

main = do
  input <- parseInput <$> readFile "input"
  print $ fuel (median, median) (\x xn -> abs (x - xn)) input
  print $ fuel ((+) (-2) . mean, (+) 2 . mean) (\x xn -> sum [1 .. abs (x - xn)]) input
