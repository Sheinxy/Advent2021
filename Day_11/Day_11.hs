module Main where

import Data.Char
import Data.Matrix

parseInput :: String -> Matrix Int
parseInput = fromLists . map (map digitToInt) . lines

getNeighbours :: Matrix Int -> (Int, Int) -> [(Int, Int)]
getNeighbours m (x, y) = filter inBounds [(x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1], i /= 0 || j /= 0]
  where inBounds (x, y) = x > 0 && x <= nrows m && y > 0 && y <= ncols m

increaseEnergy :: Matrix Int -> Matrix Int
increaseEnergy m = foldl (\x y -> setElem (x ! y + 1) y x) m [(i, j) | i <- [1 .. nrows m], j <- [1 .. ncols m]]

findFlashing :: Matrix Int -> [(Int, Int)]
findFlashing m = filter ((> 9) . (m !)) [(i, j) | i <- [1 .. nrows m], j <- [1 .. ncols m]]

performFlash :: (Matrix Int, Int) -> (Int, Int) -> (Matrix Int, Int)
performFlash (m, a) p = foldl performFlash (flashed, a + 1) propagatingFlash
  where neighbours = getNeighbours m p
        propagatingFlash = filter ((== 9) . (m !)) neighbours
        flashed = foldl (\x y -> setElem (x ! y + 1) y x) m neighbours

performStep :: (Matrix Int, Int) -> (Matrix Int, Int)
performStep (m, a) = (energyReset, flashed)
  where increased = increaseEnergy m
        (postFlash, flashed) = foldl performFlash (increased, a) $ findFlashing increased
        energyReset = foldl (\x y -> setElem 0 y x) postFlash $ findFlashing postFlash

run :: Matrix Int -> [(Matrix Int, Int)]
run m = iterate performStep (m, 0)

runFor :: Int -> Matrix Int -> (Matrix Int, Int)
runFor n m = (run m) !! n

flashesAfter :: Int -> Matrix Int -> Int
flashesAfter n = snd . runFor n

flashedSync :: Matrix Int -> Bool
flashedSync m = all ((== 0) . (m !)) [(i, j) | i <- [1 .. nrows m], j <- [1 .. ncols m]]

stepsTillSync :: Matrix Int -> Int
stepsTillSync = length . takeWhile (not . flashedSync . fst) . run

main = do
  input <- parseInput <$> readFile "input"
  print $ flashesAfter 100 input
  print $ stepsTillSync input
