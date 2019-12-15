import System.IO
import Data.List
import Data.Char
import Data.Maybe

data PV = PV Int Int deriving (Eq,Show)

parseCoord s = let
        s' = filter (\c -> c == ' ' || isDigit c || c == '-') s
        (x, _:xs) = break (==' ') s'
        (y, _:z) = break (==' ') xs
    in map read [x,y,z]

parse = (map (map (\p -> PV p 0))) . transpose . (map parseCoord) . lines


simulate :: [PV] -> PV
simulate ((PV p v):rest) = let
        v' = v + sum [signum (op - p) | (PV op _) <- rest]
        p' = p + v'
    in (PV p' v')

iterAll :: [a] -> [a] -> [[a]]
iterAll xs [y] = [y:xs]
iterAll xs (y:ys) = (y:(xs++ys)):iterAll (y:xs) ys

timestep :: [PV] -> [PV]
timestep pv = map simulate (iterAll [] pv)

asum :: [Int] -> Int
asum = sum . (map abs)

energy :: [PV] -> Int
energy pv = let
        pe = asum (map (\(PV p _) -> p) pv)
        ke = asum (map (\(PV _ v) -> v) pv)
    in pe * ke

part1 s = let
        s' = (iterate (map timestep) s) !! 100
    in sum (map energy (transpose s'))

findCycle :: [PV] -> Int
findCycle pv = (fromJust $ elemIndex pv (tail (iterate timestep pv))) + 1

part2 s = let
        c = map findCycle s
        n = foldl1 lcm c
    in n

main = do
    raw <- readFile "input"
    let input = parse raw
    print $ part1 input
    print $ part2 input
