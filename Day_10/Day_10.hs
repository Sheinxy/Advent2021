module Main where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

corruptionScore :: Map Char Int
corruptionScore = Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

completionScore :: Map Char Int
completionScore = Map.fromList $ zip [')', ']', '}', '>'] [1 .. ]

matching :: Map Char Char
matching = Map.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

findCorruption :: String -> (Char, Bool)
findCorruption = corruption []
  where corruption _ [] = ('\0', False)
        corruption expected (x:xs)
          | Map.member x matching = corruption (matching Map.! x : expected) xs
          | not (null expected) && head expected == x = corruption (tail expected) xs
          | otherwise = (x, True)

syntaxErrorScore :: [String] -> Int
syntaxErrorScore = sum . map ((corruptionScore Map.!) . fst) . filter snd . map findCorruption

getIncomplete :: [String] -> [String]
getIncomplete = filter (not . snd . findCorruption)

completeLine :: String -> [Char]
completeLine = complete []
  where complete expected [] = expected
        complete expected (x:xs)
          | Map.member x matching = complete (matching Map.! x : expected) xs
          | otherwise = complete (tail expected) xs

getCompletionScore :: String -> Int
getCompletionScore = foldl (\x y -> 5 * x + completionScore Map.! y) 0 . completeLine

middleCompletionScore :: [String] -> Int
middleCompletionScore xs = scores !! (length scores `div` 2)
  where scores = sort . map getCompletionScore . getIncomplete $ xs

main = do
  input <- lines <$> readFile "input"
  print $ syntaxErrorScore input
  print $ middleCompletionScore input
