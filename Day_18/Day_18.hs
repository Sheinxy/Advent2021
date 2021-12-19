module Main where

import Data.Char

data Pair = Value {value :: Int} | Pair {left :: Pair, right :: Pair} deriving (Eq)

instance Show Pair where
  show (Value a) = show a
  show (Pair left right) = "[" ++ show left ++ "," ++ show right ++ "]"

getMatchingBrackets :: String -> (String, String)
getMatchingBrackets str = let (s, r) = matching 1 . tail $ str in ('[' : s, r)
  where matching 0 rest = ("", rest)
        matching n (']' : rest) = let (s, r) = matching (n - 1) rest in (']' : s, r)
        matching n ('[' : rest) = let (s, r) = matching (n + 1) rest in ('[' : s, r)
        matching n (c : rest) = let (s, r) = matching n rest in (c : s, r)

readPair :: String -> Pair
readPair s = Pair left right
    where (leftStr, rL) = if s !! 1 == '[' then getMatchingBrackets (tail s) else span isDigit (tail s)
          (rightStr, rest) = if rL !! 1 == '[' then getMatchingBrackets (tail rL) else span isDigit (tail rL)
          left = if head leftStr == '[' then readPair leftStr else Value (read leftStr)
          right = if head rightStr == '[' then readPair rightStr else Value (read rightStr)

isPair :: Pair -> Bool
isPair (Value _) = False
isPair _ = True

magnitude :: Pair -> Int
magnitude (Value a) = a
magnitude (Pair left right) = 3 * magnitude left + 2 * magnitude right

addLeft :: Int -> Pair -> Pair
addLeft n (Value a) = Value (a + n)
addLeft n (Pair left right) = Pair (addLeft n left) right

addRight :: Int -> Pair -> Pair
addRight n (Value a) = Value (a + n)
addRight n (Pair left right) = Pair left (addRight n right)

reduce :: Pair -> Pair
reduce p =  snd . head . dropWhile fst . iterate step $ (True, p) 
  where explode _ (Value n) = (False, 0, 0, Value n)
        explode n (Pair l r)
          | n >= 4 && not (isPair l) && not (isPair r) = (True, value l, value r, Value 0)
          | otherwise = (expl || expr, if expr then 0 else lv, if expl then 0 else rv, Pair nl nr)
          where (expl, lv1, rv1, nl1) = explode (n + 1) l
                (expr, lv, rv, nr1) = if expl then (False, lv1, rv1, r) else explode (n + 1) r
                nl = if not expr then nl1 else addRight lv nl1
                nr = if not expl then nr1 else addLeft rv nr1
        split (Value n)
          | n >= 10 = let h = n `div` 2 in (True, Pair (Value h) (Value (n - h)))
          | otherwise = (False, Value n)
        split (Pair l r) = (hasSplit, Pair nl nr)
          where (hasSplit1, nl) = split l
                (hasSplit, nr) = if hasSplit1 then (hasSplit1, r) else split r
        step (_, p) = (action, np)
          where (action1, _, _, np1) = explode 0 p
                (action, np) = if action1 then (action1, np1) else split np1

add :: Pair -> Pair -> Pair
add p1 p2 = reduce (Pair p1 p2)

sumPairs :: [Pair] -> Pair
sumPairs = foldl1 (add)

largestMagnitude :: [Pair] -> Int
largestMagnitude ps = maximum [magnitude (p1 `add` p2) | p1 <- ps, p2 <- ps, p1 /= p2]

parseInput :: String -> [Pair]
parseInput = map readPair . lines

main = do
  input <- parseInput <$> readFile "input"
  print $ magnitude $ sumPairs input
  print $ largestMagnitude input
