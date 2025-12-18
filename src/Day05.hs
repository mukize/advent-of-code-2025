import Data.List (foldl')
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)

main :: IO ()
main = do
  input <- readFile "inputs/Day05.txt"
  print $ solve1 $ parseInput input
  print $ solve2 $ parseInput input

type Range = (Int, Int)
type ID = Int

parseInput :: String -> ([Range], [ID])
parseInput = split [] . lines
 where
  split a ("" : xs) = (a, map read xs)
  split a (x : xs) = split (a ++ [parseRange x]) xs
  split a _ = (a, [])
  parseRange r = toTuple $ map read $ splitOn "-" r
  toTuple [a, b] = (a, b)

solve1 :: ([Range], [ID]) -> Int
solve1 (ranges, ids) = length $ filter id $ map isFresh ids
 where
  isFresh :: Int -> Bool
  isFresh x = any inRange ranges
   where
    inRange (f, s) = x >= f && x <= s

solve2 :: ([Range], [ID]) -> Int
solve2 (ranges, _) = foldl' (\acc (f, s) -> acc + s - f + 1) 0 $ foldl' mergeIntoRanges [] ranges

mergeIntoRanges :: [Range] -> Range -> [Range]
mergeIntoRanges = mergeIntoRanges' []

mergeIntoRanges' :: [Range] -> [Range] -> Range -> [Range]
mergeIntoRanges' acc [] y = acc ++ [y]
mergeIntoRanges' acc (x : xs) y
  | overlap x y = mergeIntoRanges' [] (acc ++ xs) (combine x y) -- If there is an overlap, merge and start again with the other ranges
  | otherwise = mergeIntoRanges' (acc ++ [x]) xs y -- continue
 where
  overlap a b = or [fst a <=> b, snd a <=> b, fst b <=> a, snd b <=> a]
  (<=>) a (f, s) = a >= f && a <= s
  combine (f, s) (f', s') = (min f f', max s s')
