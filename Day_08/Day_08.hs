module Main where

import Data.List
import Data.List.Split
import Data.Ord
import qualified Data.Map as Map

type Entry = ([String], [String])

parseLine :: String -> Entry
parseLine line = (input, output)
  where split = splitOn " | " line
        input = words . head $ split
        output = words . concat . tail $ split

parseInput :: String -> [Entry]
parseInput = map parseLine . lines

isUnique :: String -> Bool
isUnique s = l == 2 || l == 3 || l == 4 || l == 7
  where l = length s

countUniques :: [Entry] -> Int
countUniques = length . filter isUnique . concat . map snd

findMapping :: [String] -> Map.Map String Int
findMapping [one, seven, four, x1, x2, x3, x4, x5, x6, eight] = Map.fromList $ zip [zero, one, two, three, four, five, six, seven, eight, nine] [0 .. ]
  where findWith f = head . filter f 
        two = findWith ((==) 7 . length . union four) [x1, x2, x3]
        three = findWith ((==) 5 . length . union one) [x1, x2, x3]
        five = findWith (\x -> x /= three && x /= two) [x1, x2, x3]
        six = findWith ((==) 7 . length . union one)  [x4, x5, x6]
        nine = findWith ((==) 6 . length . union four) [x4, x5, x6]
        zero = findWith (\x -> x /= six && x /= nine) [x4, x5, x6]

outputValue :: Entry -> Int
outputValue (input, output) = foldl (\x y -> 10 * x + dm Map.! y) 0 $ map sort output
  where dm = findMapping . sortBy (comparing length) $ map sort input

sumAllOutput :: [Entry] -> Int
sumAllOutput = sum . map outputValue

main = do
  input <- parseInput <$> readFile "input"
  print $ countUniques input
  print $ sumAllOutput input
