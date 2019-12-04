hasrepeat :: String -> Bool
hasrepeat [x] = False
hasrepeat (x:y:xs) = (x == y) || hasrepeat (y:xs)

increasing :: String -> Bool
increasing [x] = True
increasing (x:y:xs) = (y >= x) && increasing (y:xs)

isvalid :: Int -> Bool
isvalid x = let
        s = show x
    in (hasrepeat s) && (increasing s)

part1 (from, to) = length (filter isvalid [from .. to])

main = do
    let input = (246515, 739105)
    print $ part1 input
