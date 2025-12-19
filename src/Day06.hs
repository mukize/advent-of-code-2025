{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Bifunctor (Bifunctor (second))
import Data.Char (isSpace)
import Data.List (foldl')
import Text.Show.Pretty (pPrint)

main :: IO ()
main = do
  input <- readFile "inputs/Day06.txt"
  pPrint $ solve $ parseInput1 input
  pPrint $ solve $ parseInput2 input

type Terms = [Int]
data Operator = Add | Mul
  deriving (Show, Eq)
type Equation = (Operator, Terms)

parseInput1 :: String -> [Equation]
parseInput1 input = map (toEquation . reverse) $ foldl' groupElements [] $ map words $ lines input

parseInput2 :: String -> [Equation]
parseInput2 input = map (toEquation . seperateSign . reverse) $ groupEquations $ foldl' groupElements [] $ lines input
 where
  seperateSign (x : xs) = [sign] : term : xs
   where
    (sign, term) = second reverse $ uncons (reverse x)
    uncons (a : as) = (a, as)
  groupEquations = groupEquations' [] []
  groupEquations' accEq currEq [] = currEq : accEq
  groupEquations' accEq currEq (x : xs)
    | all isSpace x = groupEquations' (currEq : accEq) [] xs
    | otherwise = groupEquations' accEq (filter (not . isSpace) x : currEq) xs

groupElements :: [[a]] -> [a] -> [[a]]
groupElements = groupElements' []

groupElements' :: [[a]] -> [[a]] -> [a] -> [[a]]
groupElements' acc (x : xs) (y : ys) = groupElements' (acc ++ [x ++ [y]]) xs ys
groupElements' acc [] (y : ys) = groupElements' (acc ++ [[y]]) [] ys
groupElements' acc _ _ = acc

toEquation :: [String] -> Equation
toEquation (x : xs)
  | x == "+" = (Add, map read xs)
  | x == "*" = (Mul, map read xs)
  | otherwise = error "Invalid operator"
toEquation _ = error "Invalid equation"

solve :: [Equation] -> Int
solve = sum . map step
 where
  step (Add, x) = foldl' (+) 0 x
  step (Mul, x) = foldl' (*) 1 x
