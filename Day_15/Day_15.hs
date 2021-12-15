module Main where

import Data.Char
import Data.List
import Data.Matrix
import Data.Ord

thd :: (a, a, a) -> a
thd (_, _, e) = e

parseInput :: String -> Matrix Int
parseInput = fromLists . map (map digitToInt) . lines

getNeighbours :: (Int, Int) -> Matrix Int -> [(Int, Int)]
getNeighbours (x, y) m = filter inBounds $ [(x, y + 1), (x + 1, y), (x - 1, y), (x, y - 1)]
  where inBounds (x, y) = x > 0 && x <= nrows m && y > 0 && y <= ncols m

findMinimumRisk :: Matrix Int -> Int
findMinimumRisk mat = dijkstra [(nrows mat, ncols mat, 0)] dis
  where maxRisk = sum . toList $ mat
        dis = setElem 0 (nrows mat, ncols mat) $ matrix (nrows mat) (ncols mat) (const maxRisk)
        dijkstra [] dis = dis ! (1, 1)
        dijkstra ((x, y, d) : st) dis = dijkstra st' dis'
          where treatNeighbour (x, y, d) (st, dis) (x', y')
                  | dis ! (x', y') <= dis ! (x, y) + mat ! (x, y) = (st, dis)
                  | otherwise = (st', dis')
                  where st1 = if dis ! (x', y') /= maxRisk then delete (x', y', dis ! (x', y')) st else st
                        dis' = setElem (dis ! (x, y) + mat ! (x, y)) (x', y') dis
                        st' = sortBy (comparing thd) ((x', y', dis' ! (x', y')) : st1)
                neighbours = getNeighbours (x, y) mat
                (st', dis') = foldl (treatNeighbour (x, y, d)) (st, dis) neighbours 
                
getFullMap :: Matrix Int -> Matrix Int
getFullMap mat = matrix (r * 5) (c * 5) generator
  where (r, c) = (nrows mat, ncols mat)
        generator (x, y) = i
          where im = mat ! ((x - 1) `mod` r + 1, (y - 1) `mod` c + 1)
                repeats = (x - 1) `div` r + (y - 1) `div` c
                i = (im + repeats - 1) `mod` 9 + 1

main = do
  input <- parseInput <$> readFile "input"
  print $ findMinimumRisk input
  print $ findMinimumRisk . getFullMap $ input
