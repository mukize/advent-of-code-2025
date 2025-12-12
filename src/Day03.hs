main :: IO ()
main = do
  input <- readFile "inputs/Day03.txt"
  print $ solve1 $ lines input
  print $ solve2 $ lines input

solve1 :: [String] -> Int
solve1 banks = sum $ map (read . largestJoltage) banks

solve2 :: [String] -> Int
solve2 banks = sum $ map (read . largestJoltageN 12) banks

largestJoltage :: String -> String
largestJoltage banks = step banks ""
 where
  step [x] [h] = [h, x]
  step [x] [h, t] = [h, max t x]
  step (x : xs) [] = step xs [x]
  step (x : xs) [h] = step xs (if x > h then [x] else [h, x])
  step (x : xs) [h, t] = step xs (if x > h then [x] else [h, max t x])
  step b a = error $ show (b, a)

largestJoltageN :: Int -> String -> String
largestJoltageN n bank = step bank ""
 where
  step [] acc = acc
  step (x : xs) acc = step xs $ compareX acc ""
   where
    compareX :: String -> String -> String
    compareX [] acc'
      | length acc' == n = acc'
      | otherwise = acc' ++ [x]
    compareX (y : ys) acc'
      | x > y && (length acc' + 1 + length xs >= n) = acc' ++ [x]
      | otherwise = compareX ys (acc' ++ [y])
