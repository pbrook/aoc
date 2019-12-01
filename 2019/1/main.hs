import System.IO

moduleFuel :: Int -> Int

moduleFuel mass = (mass `div` 3) - 2

hMain lines = sum (map moduleFuel lines)

parse = (map read) . lines

main = do
    input <- readFile "input"
    let result = hMain (parse input)
    print result
