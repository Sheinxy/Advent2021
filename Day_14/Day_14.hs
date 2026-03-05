module Main where

import Data.List
import Data.List.Split
import qualified Data.Map as Map

type Pairs = Map.Map String Int
type InsertionMap = Map.Map String Char
type Counter = Map.Map Char Int

parseInput :: String -> (Pairs, InsertionMap, Counter)
parseInput input = (pairs, insertion, counter)
  where inputLines = lines input
        polymer = head inputLines
        insertion = Map.fromList . map (\[a, b] -> (a, head b)) . map (splitOn " -> ") . drop 2 $ inputLines
        counter = foldl (\m c -> Map.insertWith (+) c 1 m) Map.empty polymer
        pairsStrings = filter (\p -> Map.member p insertion) . zipWith (\a b -> [a, b]) polymer . tail $ polymer
        pairs = foldl (\m p -> Map.insertWith (+) p 1 m) Map.empty pairsStrings

treatPair :: InsertionMap -> Pairs -> (Pairs, Counter) -> String -> (Pairs, Counter)
treatPair insertion oldPairs (pairs, counter) pair = (pairs', counter')
  where c = insertion Map.! pair
        n = oldPairs Map.! pair
        pairsStrings = filter (\p -> Map.member p insertion) [head pair : [c], c : tail pair]
        pairs' = foldl (\m p -> Map.insertWith (+) p n m) pairs pairsStrings
        counter' = Map.insertWith (+) c n counter

step :: InsertionMap -> (Pairs, Counter) -> (Pairs, Counter)
step insertion (pairs, counter) = foldl (treatPair insertion pairs) (Map.empty, counter) $ Map.keys pairs

run :: (Pairs, InsertionMap, Counter) -> [(Pairs, Counter)]
run (pairs, insertion, counter) = iterate (step insertion) (pairs, counter)

quantitiesAfter :: Int -> (Pairs, InsertionMap, Counter) -> Int
quantitiesAfter n input = most - least
  where occurences = sort . map snd . Map.toList . snd $ run input !! n
        most = last occurences
        least = head occurences
        
main = do
  input <- parseInput <$> readFile "input"
  print $ quantitiesAfter 10 input
  print $ quantitiesAfter 40 input
