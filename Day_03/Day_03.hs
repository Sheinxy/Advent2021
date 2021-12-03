module Main where

import Data.List
import Data.Ord
import Data.Char

parseInput :: [String] -> [[Int]]
parseInput = map $ map digitToInt

getMostCommon :: Ord a => [a] -> a
getMostCommon = head . maximumBy (comparing length) . group . sort

getLeastCommon :: Ord a => [a] -> a
getLeastCommon = head . minimumBy (comparing length) . group . sort

getNumber :: [Int] -> Int
getNumber = foldl (\x y -> 2 * x + y) 0

gamma :: [[Int]] -> Int
gamma = getNumber . map getMostCommon . transpose

epsilon :: [[Int]] -> Int
epsilon = getNumber . map getLeastCommon . transpose

filterAt :: Ord a => [[a]] -> Int -> ([a] -> a) -> [[a]]
filterAt l n f = filterWithDigit l (n, f $ digits !! n)
  where filterWithDigit l (n, d) = filter ((==) d . flip (!!) n) l
        digits = transpose l

oxygen :: [[Int]] -> Int
oxygen l = _oxygen l 0
  where _oxygen [a] _ = getNumber a 
        _oxygen l n = _oxygen (filterAt l n getMostCommon) (n + 1)

co2 :: [[Int]] -> Int
co2 l = _co2 l 0
  where _co2 [a] _ = getNumber a 
        _co2 l n = _co2 (filterAt l n getLeastCommon) (n + 1)

main :: IO ()
main = do
  input <- parseInput . lines <$> readFile "input"
  let (g, e) = (gamma input, epsilon input)
  let (c, o) = (oxygen input, co2 input)
  print (g * e)
  print (c * o)
