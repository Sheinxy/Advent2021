module Main where

increments :: [Int] -> Int
increments l = length . filter (== True) . zipWith (<) l $ tail l

sumThree :: [Int] -> [Int]
sumThree l = zipWith3 (\x y z -> x + y + z) l (tail l) (drop 2 l)

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  content <- readFile "input"
  let input = map readInt . lines $ content
  print $ increments input
  print $ increments $ sumThree input
