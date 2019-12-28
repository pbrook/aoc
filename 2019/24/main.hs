import Data.Array
import Data.List
import Data.Char
import Data.Maybe
import Data.Bits

import Debug.Trace

parseChar '.' = 0
parseChar '#' = 1

data Dir = DUp | DDown | DLeft | DRight
type Grid = Array (Int, Int) Int
data GridPoint = GridPoint {gParent :: Grid, gMe :: Grid, gChild :: Grid}

parse :: String -> Grid
parse = listArray ((1,1),(5,5)) . map parseChar . filter (/= '\n')

sumRow y g = sum [g ! (x, y) | x <- [1..5]]
sumCol x g = sum [g ! (x, y) | y <- [1..5]]

getNext :: GridPoint -> Dir -> (Int, Int) -> Int
getNext g DUp (_, 1) = (gParent g) ! (3, 2)
getNext g DDown (_, 5) = (gParent g) ! (3, 4)
getNext g DLeft (1, _) = (gParent g) ! (2, 3)
getNext g DRight (5, _) = (gParent g) ! (4, 3)
getNext g DUp (3, 4) = sumRow 5 (gChild g)
getNext g DDown (3, 2) = sumRow 1 (gChild g)
getNext g DLeft (4, 3) = sumCol 5 (gChild g)
getNext g DRight (2, 3) = sumCol 1 (gChild g)
getNext g DUp (x, y) = (gMe g) ! (x, y-1)
getNext g DDown (x, y) = (gMe g) ! (x, y+1)
getNext g DLeft (x, y) = (gMe g) ! (x-1, y)
getNext g DRight (x, y) = (gMe g) ! (x+1, y)

newBug _ (3,3) = 0
newBug g p = let
        n = sum [getNext g d p | d <- [DUp, DDown, DLeft, DRight]]
    in case (gMe g) ! p of
        0 -> case n of
            1 -> 1
            2 -> 1
            _ -> 0
        1 -> case n of
            1 -> 1
            _ -> 0

emptyGrid :: Grid
emptyGrid = listArray ((1,1),(5,5)) (replicate 25 0)

stepOne :: Grid -> Grid -> Grid -> Grid
stepOne up me down = let
        gp = GridPoint up me down
    in listArray ((1,1),(5,5)) (map (newBug gp) (indices me))

stepMany :: [Grid] -> [Grid]
stepMany [a, b] = let
        new = stepOne a b emptyGrid
        next = stepOne b emptyGrid emptyGrid
    in if next == emptyGrid
        then [new]
        else [new, next]
stepMany (a:b:c:rest) = let
        new = stepOne a b c
    in new:(stepMany (b:c:rest))

stepAll :: [Grid] -> [Grid]
stepAll g@(a:_) = let
        new = stepOne emptyGrid emptyGrid a
        rest = stepMany (emptyGrid:g)
    in if new == emptyGrid
        then rest
        else new:rest

multiStep 0 g = g
multiStep n g = multiStep (n-1) (stepAll g)

bugChar 0 = '.'
bugChar 1 = '#'
split5 [] = Nothing
split5 s = Just (splitAt 5 s)
render grid = let
        s = unlines $ unfoldr split5 $ map bugChar $ elems grid
    in trace ((take 14 s) ++ '?':(drop 15 s)) grid

part2 grid = sum $ map sum $ multiStep 200 [grid]

main = do
    raw <- readFile "input"
    let input = parse raw
    print $ part2 input
