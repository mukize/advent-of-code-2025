{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

main :: IO ()
main = do
  input <- readFile "inputs/Day01.txt"
  print $ solve (lines input) 50 0

solve :: [String] -> Int -> Int -> Int
solve [] _ zeroClicks = zeroClicks
solve ((direction : distance) : rest) pointer zeroClicks = solve rest pointer' pointZeroCount'
 where
  (wrapped, pointer') = correctPointerOverflow $ case direction of
    'L' -> pointer - distanceInt
    'R' -> pointer + distanceInt
    _ -> pointer
  pointZeroCount' = zeroClicks + clicks
  clicks = case wrapped of
    0 -- did not wrap
      | pointer' == 0 -> 1
      | otherwise -> 0
    x -- wrapped around
      | underflowedFromZero && landedOnZero -> abs x -- underflowed from zero & landed on zero
      | underflowed && landedOnZero -> abs x + 1 -- underflowed & landed on zero
      | underflowedFromZero -> abs x - 1 -- underflowed from zero only
      | otherwise -> abs x -- underflowed/overflowed and passed zero
  distanceInt = read distance :: Int
  underflowed = wrapped < 0
  underflowedFromZero = underflowed && pointer == 0
  landedOnZero = pointer' == 0

correctPointerOverflow :: Int -> (Int, Int)
correctPointerOverflow pointer = pointer `divMod` 100
