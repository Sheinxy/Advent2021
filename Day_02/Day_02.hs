module Main where

move :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
move (height, depth, aim) ("forward", amt) = (height + amt, depth + aim * amt, aim)
move (height, depth, aim) ("down", amt) = (height, depth, aim + amt)
move (height, depth, aim) (_, amt) = (height, depth, aim - amt)

moveAround :: [(String, Int)] -> (Int, Int, Int)
moveAround = foldl move (0, 0, 0)

parseLine :: [String] -> (String, Int)
parseLine l = (dir, read amt)
  where (dir, amt) = head . zip l $ tail l

main = moveAround <$> map (parseLine . words) . lines <$> readFile "input" >>= 
  \(h, d, a) -> print (h * a) >> print (h * d)
