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

flatten :: [Char] -> [Char] -> [Char]
flatten [] [] = []
flatten (x:xs) (y:ys) =
    (if x /= '2' then x else y):(flatten xs ys)

pixelate :: Char -> Char
pixelate '0' = ' '
pixelate '1' = 'X'
pixelate '2' = '?'
pixelate c = c

rectangle :: Int -> String -> String
rectangle _ [] = []
rectangle w s = let
        (a, b) = splitAt w s
    in a ++ ('\n':rectangle w b)

part2 layers = let
        num = foldl1 flatten layers
        s = map pixelate num
    in rectangle 25 s

main = do
    raw <- readFile "input"
    let input = parse (25 * 6) (filter (not . isSpace) raw)
    print $ part1 input
    putStr $ part2 input
