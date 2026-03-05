module Main where

import Data.Char
import Data.List
import Data.Matrix

parseInput :: String -> Matrix Int
parseInput = fromLists . map (map digitToInt) . lines 

getAdjacent :: Matrix Int -> (Int, Int) -> [(Int, Int)]
getAdjacent m (x, y) = filter inBounds [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
  where inBounds (x, y) = x > 0 && x <= nrows m && y > 0 && y <= ncols m

getAdjacentValues :: Matrix Int -> (Int, Int) -> [Int]
getAdjacentValues m p = map (m !) $ getAdjacent m p

getLowPoints :: Matrix Int -> [(Int, Int)]
getLowPoints m = filter (\x -> all (> m ! x) (getAdjacentValues m x)) [(i, j) | i <- [1 .. nrows m], j <- [1 .. ncols m]]

getLowPointsValues :: Matrix Int -> [Int]
getLowPointsValues m = map (m !)  $ getLowPoints m

sumRisks :: Matrix Int -> Int
sumRisks = sum . map (+ 1) . getLowPointsValues

getBasinSize :: Matrix Int -> (Int, Int) -> Int
getBasinSize m p = snd $ getSize (m, 0) p
  where getSize (m, a) p = foldl getSize (marked, a + 1) unvisitedAdj
          where unvisitedAdj = filter ((/= 9) . (m !)) $ getAdjacent m p
                marked = foldl (flip $ setElem 9) (setElem 9 p m) unvisitedAdj

getBasinSizes :: Matrix Int -> [Int]
getBasinSizes m = map (getBasinSize m) $ getLowPoints m

prodThreeLargest :: Matrix Int -> Int
prodThreeLargest = product . take 3 . reverse . sort . getBasinSizes

main = do
  input <- parseInput <$> readFile "input"
  print $ sumRisks input
  print $ prodThreeLargest input
