module Main where

import Data.List

increments :: [Int] -> Int
increments l = length . filter (== True) . zipWith (<) l $ tail l

sumThree :: [Int] -> [Int]
sumThree = map (sum . take 3) . takeWhile ((<=) 3 . length) . tails

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  content <- readFile "input"
  let input = map readInt . lines $ content
  print $ increments input
  print $ increments $ sumThree input
