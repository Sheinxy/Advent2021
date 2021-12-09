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
findMapping [one, seven, four, x1, x2, x3, x4, x5, x6, eight] = Map.fromList [(one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6), (seven, 7), (eight, 8), (nine, 9), (zero, 0)]
  where two = head . filter((==) 7 . length . union four) $ [x1, x2, x3]
        three = head . filter ((==) 5 . length . union one) $ [x1, x2, x3]
        five = head . filter (\x -> x /= three && x /= two) $ [x1, x2, x3]
        six = head . filter ((==) 7 . length . union one)  $ [x4, x5, x6]
        nine = head . filter ((==) 6 . length . union (union five one)) $ [x4, x5, x6]
        zero = head . filter (\x -> x /= six && x /= nine) $ [x4, x5, x6]

outputValue :: Entry -> Int
outputValue (input, output) = foldl (\x y -> 10 * x + dm Map.! y) 0 $ map sort output
  where dm = findMapping $ sortBy (comparing length) $ map sort input

sumAllOutput :: [Entry] -> Int
sumAllOutput = sum . map outputValue

main = do
  input <- parseInput <$> readFile "input"
  print $ countUniques input
  print $ sumAllOutput input
