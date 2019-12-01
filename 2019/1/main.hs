import System.IO

parse = (map read) . lines

moduleFuel :: Int -> Int
moduleFuel mass = (mass `div` 3) - 2

part1 modules = sum (map moduleFuel modules)

recursiveFuel x
    | fuel <= 0 = 0
    | otherwise = fuel + recursiveFuel fuel
    where fuel = moduleFuel x

part2 modules = sum (map recursiveFuel modules)

main = do
    raw <- readFile "input"
    let input = parse raw
    print $ part1 input
    print $ part2 input
