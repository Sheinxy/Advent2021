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

fuelMed :: [Int] -> Int
fuelMed xs = sum . map (abs . (-) med) $ xs
  where med = median xs

fuelTo :: Int -> [Int] -> Int
fuelTo x = sum . map (\xn -> sum [1 .. abs (xn - x)])

fuel :: [Int] -> Int
fuel xs = minimum [fuelTo x sorted | x <- [head sorted .. last sorted]]
  where sorted = sort xs

main = do
  input <- parseInput <$> readFile "input"
  print $ fuelMed input
  print $ fuel input
