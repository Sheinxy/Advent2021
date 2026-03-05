module Main where

import Data.Map (Map, (!), fromList)

type Bits = [Int]
data Packet = 
  Literal {len :: Int, version :: Int, pType :: Int, value :: Int} | 
  Operator {len :: Int, version :: Int, pType :: Int, sub :: [Packet]} deriving (Show, Eq)

hexToBit :: Char -> Bits
hexToBit = (hex2bit !)
  where hex2bit = fromList $ zip "0123456789ABCDEF" 
                            [[n `div` 8 `mod` 2, n `div` 4 `mod` 2, n `div` 2 `mod` 2, n `mod` 2] | 
                              n <- [0 .. 15]]

bitToInt :: Bits -> Int
bitToInt = foldl (\x y -> 2 * x + y) 0

literalPacket :: Bits -> Packet
literalPacket bits = Literal packetlen version 4 value
  where version = bitToInt . take 3 $ bits
        getGroups [] = []
        getGroups l
          | head l == 0 = [take 5 l]
          | otherwise = take 5 l : getGroups (drop 5 l)
        groups = getGroups (drop 6 bits)
        packetlen = 6 + (length . concat $ groups)
        value = bitToInt . concatMap tail $ groups

findSubsTypeOne :: Bits -> [Packet]
findSubsTypeOne bits = packets
  where subLen = bitToInt . take 15 . drop 7 $ bits
        subBits = take subLen . drop 22 $ bits
        packets = findPackets True subBits

findSubsTypeTwo :: Bits -> [Packet]
findSubsTypeTwo bits = reverse packets
  where numPacket = bitToInt . take 11 . drop 7 $ bits
        takePacket (n, ps, bs) = (n - 1, p : ps, l)
          where (p, l) = findPacket True bs
        (_, packets, _) = head . dropWhile (\(n, _, _) -> n /= 0) . iterate takePacket $ (numPacket, [], drop 18 bits)

operatorPacket :: Bits -> Packet
operatorPacket bits = Operator packetlen version ptype sub
  where version = bitToInt . take 3 $ bits
        ptype = bitToInt . take 3 . drop 3 $ bits
        id = head . take 1 . drop 6 $ bits
        sub = if id == 0 then findSubsTypeOne bits else findSubsTypeTwo bits
        packetlen = 7 + (if id == 0 then 15 else 11) + foldl (\x p -> x + len p) 0 sub

findPacket :: Bool -> Bits -> (Packet, Bits)
findPacket sub bits = (packet, remaining)
  where packetType = bitToInt . take 3 . drop 3 $ bits
        packet = if packetType == 4 then literalPacket bits else operatorPacket bits
        toDrop = if sub || (len packet) `mod` 4 == 0 then len packet else (4 * ((len packet) `div` 4 + 1))
        remaining = drop toDrop bits

findPackets :: Bool -> Bits -> [Packet]
findPackets sub bits = reverse . fst . head . dropWhile ((>= 11) . length . snd) . iterate nextPacket $ ([], bits)
  where nextPacket (ps, bs) = (p : ps, bl)
          where (p, bl) = findPacket sub bs

parseInput :: String -> [Packet]
parseInput = findPackets False . concatMap hexToBit . head . lines

sumVersions :: [Packet] -> Int
sumVersions = foldl addVersion 0
  where addVersion acc (Literal _ v _ _) = acc + v
        addVersion acc (Operator _ v _ s) = acc + v + sumVersions s

compute :: Packet -> Int
compute (Literal _ _ _ v) = v
compute (Operator _ _ t sub) = (packetAction ! t) sub
  where packetAction = fromList $ zip (filter (/= 4) [0 .. 7]) [sum . map compute, 
            product . map compute, minimum . map compute, maximum . map compute,
            \s -> let subcom = map compute s in if head subcom > last subcom then 1 else 0,
            \s -> let subcom = map compute s in if head subcom < last subcom then 1 else 0,
            \s -> let subcom = map compute s in if head subcom == last subcom then 1 else 0]

main = do
  input <- parseInput <$> readFile "input"
  print $ sumVersions input
  print $ compute . head $ input
