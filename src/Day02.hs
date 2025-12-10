{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List.Split (chunksOf, splitOn)

main :: IO ()
main = do
  input <- readFile "inputs/Day02.txt"
  let ranges = map (convertToIntRange . splitOn "-") $ splitOn "," input
  print $ solve1 ranges
  print $ solve2 ranges

convertToIntRange :: [String] -> [Int]
convertToIntRange [a, b] = [read a .. read b]

solve1 :: [[Int]] -> Int
solve1 ranges = sum $ map (sum . filter isSequence) ranges
 where
  isSequence idInt
    | odd (length idString) = False
    | firstHalf == secondHalf = True
    | otherwise = False
   where
    idString = show idInt
    (firstHalf, secondHalf) = splitAt (length idString `div` 2) idString

solve2 :: [[Int]] -> Int
solve2 ranges = sum $ map (sum . filter isSequence) ranges
 where
  isSequence idInt = any (chunksEqual . breakIdIntoChunks) divisors
   where
    idString = show idInt
    idLength = length idString
    divisors = [x | x <- [1 .. idLength - 1], (idLength `mod` x) == 0]
    breakIdIntoChunks x = chunksOf x idString
    chunksEqual (x : xs) = all (== x) xs
