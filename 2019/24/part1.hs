import Data.Array
import Data.List
import Data.Char
import Data.Maybe
import Data.Bits

import Debug.Trace

parseChar '.' = 0
parseChar '#' = 1

parse :: String -> [[Int]]
parse = map (map parseChar) . lines 

headOr [] y = y
headOr x _ = head x

stepH :: [Int] -> [Int] -> Int -> [Int] -> [Int]
stepH [] _ _ _ = []
stepH (u:us) (d:ds) l (m:rs) = let
        r = headOr rs 0
    in case m of
        0 -> case u+d+l+r of
            1 -> 1
            2 -> 1
            _ -> 0
        1 -> case u+d+l+r of
            1 -> 1
            _ -> 0
        : (stepH us ds m rs)
    
blankRow = replicate 5 0

stepV :: [Int] -> [[Int]] -> [[Int]]
stepV _ [] = []
stepV u (m:rest) = (stepH u (headOr rest blankRow) 0 m):(stepV m (rest))

step grid = stepV blankRow grid

bugChar 0 = '.'
bugChar 1 = '#'
render grid = trace (unlines (map (map bugChar) grid)) grid

type Grid = [[Int]]

bioVal grid = sum $ zipWith (\a b -> a * bit b) (concat grid) [0..]

findRepeat :: [Grid] -> Grid -> Grid
findRepeat prev grid = case find (==grid) prev of
        Nothing -> findRepeat (grid:prev) (step grid)
        Just _ -> grid

part1 = bioVal . findRepeat []

main = do
    raw <- readFile "input"
    let input = parse raw
    print $ part1 input
