module Main where

import Data.List
import Data.List.Split

type Grid = [[(Int, Bool)]]

parseInput :: [String] -> ([Int], [Grid])
parseInput input = (map read . splitOn "," $ head input, constructGrids $ tail input)
  where constructGrid = map (map (\x -> (read x :: Int, False)) . words) . take 5 
        constructGrids [] = []
        constructGrids input =  (constructGrid input) : (constructGrids $ drop 5 input)

checkWin :: Grid -> Bool
checkWin grid = checkWinRow grid || checkWinColumn grid
  where checkWinRow = any $ all ((== True) . snd)
        checkWinColumn = checkWinRow . transpose

sumUnmarked :: Grid -> Int
sumUnmarked = foldl (\x (y, b) -> x + (if b then 0 else y)) 0 . concat

mark :: Int -> Grid -> Grid
mark n = map (map (\(x, b) -> if (x == n) then (x, True) else (x, b)))

play :: [Int] -> [Grid] -> (Int, Int) -> (Int, Int)
play [] _ r = r
play nums grids (f, l)
  | last_winner = (f, head nums * sumUnmarked (head winners))
  | first_winner = play (tail nums) losers (head nums * sumUnmarked (head winners), l)
  | otherwise = play (tail nums) losers (f, l)
  where marked = map (mark $ head nums) grids
        winners = filter checkWin marked
        losers = filter (not . checkWin) marked
        last_winner = not (null winners) && null losers
        first_winner = f == 0 && not (null winners)
        
main = do
  input <- parseInput . filter (/= "") . lines <$> readFile "input"
  let (f, l) = play (fst input) (snd input) (0, 0)
  print f
  print l
