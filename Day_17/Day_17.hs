module Main where

import Data.Ord
import Data.List
import Data.List.Split

type Range = (Int, Int)

parseInput :: String -> (Range, Range)
parseInput = toTuple . map (toTuple . sort . map read . splitOn ".." .drop 2) . 
             splitOn ", " . drop (length "target area: ") . head . lines
 where toTuple [x, y] = (x, y)

allYs :: Int -> [Int]
allYs v0 = [n * (1 - n) `div` 2 + n * v0 | n <- [0 .. ]]

allXs :: Int -> [Int]
allXs v0 = moving ++ repeat (last moving)
  where moving = scanl (\x v -> x + v) 0 [v0, v0 - 1 .. 0]

possibleYs :: Range -> [Int]
possibleYs (low, hi) = filter wouldHitTarget [low .. 1000]
  where wouldHitTarget v0 = not . null . takeWhile (\n -> low <= n && n <= hi) . dropWhile (> hi) $
                            [n * (1 - n) `div` 2 + n * v0 | n <- [v0 .. ]]

possibleXs :: Range -> [Int]
possibleXs (low, hi) = filter wouldHitTarget [1 .. hi]
  where wouldHitTarget v0 = not . null . takeWhile (\n -> low <= n && n <= hi) . dropWhile (< low) .
                            scanl (\x v -> x + v) 0  $ [v0,v0 - 1 .. 0]


touchesTarget :: (Int, Int) -> (Range, Range) -> Bool
touchesTarget (vx, vy) ((lowx, hix), (lowy, hiy)) = not . null . filter inTarget . takeWhile notPassedTarget $
                                                    zip (allXs vx) (allYs vy)
  where notPassedTarget (x, y) = x <= hix && lowy <= y
        inTarget (x, y) = lowx <= x && x <= hix && lowy <= y && y <= hiy

possibleXYs :: (Range, Range) -> [(Int, Int)]
possibleXYs (xrange, yrange) = [(x, y) | x <- possiblex, y <- possibley, touchesTarget (x, y) (xrange, yrange)]
  where possiblex = possibleXs xrange
        possibley = possibleYs yrange

highestY :: (Range, Range) -> Int
highestY ranges = v0 * (1 + v0) `div` 2
  where v0 = snd . maximumBy (comparing snd) . possibleXYs $ ranges

main = do
  input <- parseInput <$> readFile "input"
  print $ highestY input
  print $ length . possibleXYs $ input
