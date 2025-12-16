import Data.Bifunctor (Bifunctor (bimap))
import GHC.List (foldr')

main :: IO ()
main = do
  input <- readFile "inputs/Day04.txt"
  -- let testRow = [[a, b, c] | a <- ['@', '.'], b <- ['@', '.'], c <- ['@', '.']]
  -- let testGrid = [unlines [a, b, c] | a <- testRow, b <- testRow, c <- testRow]
  -- let testSolve = map (solve1 . parseGrid) testGrid
  -- mapM_ putStrLn $ zipWith (++) testGrid $ map show testSolve
  print $ solve1 $ parseGrid input
  print $ solve2 $ parseGrid input

solve1 :: Grid -> Int
solve1 grid = sum $ map (length . filter (< 4) . filter (/= -1) . map count) $ gridWithCoords grid
 where
  count (1, coords) = countNeighbouringRolls grid coords
  count (_, _) = -1

solve2 :: Grid -> Int
solve2 = step []
 where
  step :: [Int] -> Grid -> Int
  step (0 : xs) _ = sum xs
  step counts grid = step (newCount : counts) newGrid
   where
    (newCount, newGrid) = foldr' step' (0, []) $ gridWithCoords grid
    step' row (count, rowAcc) = (count + newCount', rowAcc ++ [newRow])
     where
      newCount' = length $ filter (< 4) $ filter (/= -1) neighbourCountRow
      newRow = map (\x -> if x < 4 then -1 else 1) neighbourCountRow
      neighbourCountRow = map countNeighbours row
      countNeighbours (1, coords) = countNeighbouringRolls grid coords
      countNeighbours (_, _) = -1

countNeighbouringRolls :: Grid -> Coord -> Int
countNeighbouringRolls grid coord = sum $ filter (/= -1) $ map index $ neighbourCoords grid coord
 where
  index (r, c) = grid !! r !! c

neighbourCoords :: Grid -> Coord -> [Coord]
neighbourCoords g (y, x) = filter inBound neighbours
 where
  deltas = filter (/= (0, 0)) [(nx, ny) | nx <- [-1, 0, 1], ny <- [-1, 0, 1]]
  neighbours = map (bimap (+ y) (+ x)) deltas
  inBound (ny, nx) = ny >= 0 && ny < height g && nx >= 0 && nx < width g

type Grid = [Row]
type Row = [Int]
type Coord = (Int, Int)
type GridWithCoords = [[(Int, Coord)]]

height :: Grid -> Int
height = length

width :: Grid -> Int
width = length . (!! 0)

gridCoords :: Grid -> [[Coord]]
gridCoords grid = [[(y, x) | x <- [0 .. width grid - 1]] | y <- [0 .. height grid - 1]]

gridWithCoords :: Grid -> GridWithCoords
gridWithCoords g = zipWith zip g (gridCoords g)

parseGrid :: String -> Grid
parseGrid input = map (step []) $ lines input
 where
  step :: [Int] -> String -> [Int]
  step acc [] = acc
  step acc ('@' : xs) = step (acc ++ [1]) xs
  step acc (_ : xs) = step (acc ++ [-1]) xs
