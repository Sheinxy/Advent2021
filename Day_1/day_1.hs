module Main where

-- There has to be a beautifully Haskell way to do that but I couldn't find one

increments :: Int -> [Int] -> Int
increments x [] = x
increments x (_ : []) = x
increments x (a : b : l) 
  | b > a = increments (x + 1) (b : l)
  | otherwise = increments x (b : l)

sumThree :: [Int] -> [Int]
sumThree [] = []
sumThree (_ : []) = []
sumThree (_ : _ : []) = []
sumThree (a : b : c : l) = (a + b + c) : sumThree (b : c : l)

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  content <- readFile "input"
  let input = map readInt . lines $ content
  print $ increments 0 input
  print $ increments 0 $ sumThree input
