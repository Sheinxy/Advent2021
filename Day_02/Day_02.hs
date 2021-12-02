module Main where

move :: (Int, Int) -> (String, Int) -> (Int, Int)
move (height, depth) ("forward", amt) = (height + amt, depth)
move (height, depth) ("down", amt) = (height, depth + amt)
move (height, depth) (_, amt) = (height, depth - amt)

moveAround :: [(String, Int)] -> (Int, Int)
moveAround = foldl move (0, 0)

moveAndAim :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
moveAndAim (height, depth, aim) ("forward", amt) = (height + amt, depth + aim * amt, aim)
moveAndAim (height, depth, aim) ("down", amt) = (height, depth, aim + amt)
moveAndAim (height, depth, aim) (_, amt) = (height, depth, aim - amt)

moveAroundAndAim :: [(String, Int)] -> (Int, Int, Int)
moveAroundAndAim = foldl moveAndAim (0, 0, 0)

readInt :: String -> Int
readInt = read

parseLine :: [String] -> (String, Int)
parseLine l = (dir, readInt amt)
  where (dir, amt) = head . zip l $ tail l

main = do
  content <- readFile "input"
  let input = map (parseLine . words) . lines $ content
  let (h, d) = moveAround input
  print (h * d)
  let (h, d, a) = moveAroundAndAim input
  print (h * d)
