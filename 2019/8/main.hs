import System.IO
import Data.List
import Data.Char

parse _ [] = []
parse size raw = let
        (a, b) = splitAt size raw
    in a:(parse size b)

countN :: (Int, Int, Int) -> Char -> (Int, Int, Int)
countN (x0, b, x2) '0' = (x0+1, b, x2)
countN (x0, b, x2) '1' = (x0, b+1, x2)
countN (x0, b, x2) '2' = (x0, b, x2+1)

best :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
best x@(x0, x1, x2) y@(y0, y1, y2)
    | x0 < y0 = x
    | otherwise = y

part1 layers = let
        c = map (foldl countN (0,0,0)) layers
        (x0, x1, x2) = foldl1 best c
    in x1 * x2


main = do
    raw <- readFile "input"
    let input = parse (25 * 6) (filter (not . isSpace) raw)
    print $ part1 input
