module Main where

move :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
move (height, depth, aim) ("forward", amt) = (height + amt, depth + aim * amt, aim)
move (height, depth, aim) ("down", amt) = (height, depth, aim + amt)
move (height, depth, aim) (_, amt) = (height, depth, aim - amt)

moveAround :: [(String, Int)] -> (Int, Int, Int)
moveAround = foldl move (0, 0, 0)

readInt :: String -> Int
readInt = read

parseLine :: [String] -> (String, Int)
parseLine l = (dir, readInt amt)
  where (dir, amt) = head . zip l $ tail l

main = do
  content <- readFile "input"
  let input = map (parseLine . words) . lines $ content
  let (h, d, a) = moveAround input
  print (h * a)
  print (h * d)
