import Data.List

increasing :: String -> Bool
increasing [x] = True
increasing (x:y:xs) = (y >= x) && increasing (y:xs)

hasGroup :: (Int -> Bool) -> String -> Bool
hasGroup f x = any (f . length) (group x)

isvalid :: (Int -> Bool) -> Int -> Bool
isvalid f x = let
        s = show x
    in (increasing s) && (hasGroup f s)

countValid f (from, to) = length (filter (isvalid f) [from .. to])

part1 input = countValid (>= 2) input

part2 input = countValid (== 2) input

main = do
    let input = (246515, 739105)
    print $ part1 input
    print $ part2 input
