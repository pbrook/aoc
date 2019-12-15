import System.IO
import Data.List
import Data.Char

data Moon = Moon [Int] [Int] deriving Show

--parsePlanet :: String -> Planet
parseMoon s = let
        s' = filter (\c -> c == ' ' || isDigit c || c == '-') s
        (x, _:xs) = break (==' ') s'
        (y, _:z) = break (==' ') xs
    in Moon (map read [x,y, z]) [0,0,0]

parse = (map parseMoon) . lines

addGravity :: [Moon] -> Moon
addGravity [m] = m
addGravity ((Moon p v):(Moon op _):ms) = let
        m = Moon p (map (\(a, b, c) -> a + signum (c - b)) (zip3 v p op))
    in addGravity (m:ms)

move :: Moon -> Moon
move (Moon p v) = Moon (map (\(a, b) -> a + b) (zip p v)) v

iterMoon :: [Moon] -> [Moon] -> [[Moon]]
iterMoon xs [y] = [y:xs]
iterMoon xs (y:ys) = (y:(xs++ys)):iterMoon (y:xs) ys

timestep :: [Moon] -> [Moon]
timestep m = map (move.addGravity) (iterMoon [] m)

energy :: Moon -> Int
energy (Moon p v) = (asum p) * (asum v)
    where asum = sum.(map abs)

part1 m = sum $ map energy ((iterate timestep m) !! 1000)

main = do
    raw <- readFile "input"
    let input = parse raw
    print $ part1 input
