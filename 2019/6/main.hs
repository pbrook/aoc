import System.IO

data Mass = Mass String [Mass] deriving Show
data Orbit = Orbit String String

parseOrbit :: String -> Orbit
parseOrbit s =
    let (a, b) = span (/= ')') s
    in Orbit a (tail b)

parse = map parseOrbit . lines

buildMass :: [Orbit] -> String -> Mass
buildMass orbits name = let
        sName = [outer | Orbit inner outer <- orbits, inner == name]
    in Mass name (map (buildMass orbits) sName)

numSat :: Mass -> Int
numSat (Mass name s) = length s + sum (map numSat s)

totalOrbits :: Mass -> Int
totalOrbits m@(Mass name s) = numSat m + sum (map totalOrbits s)

part1 = totalOrbits

findMass :: String -> Mass -> [String]
findMass dest (Mass name s)
    | name == dest = [name]
    | otherwise = let found = concat (map (findMass dest) s)
        in case found of
            [] -> []
            _ -> name:found

uniqueOrbits :: [String] -> [String] -> Int
uniqueOrbits (x:xs) (y:ys)
    | x == y = uniqueOrbits xs ys
    | otherwise = length xs + length ys

part2 com = let
        you = findMass "YOU" com
        san = findMass "SAN" com
    in uniqueOrbits you san

main = do
    raw <- readFile "input"
    let
        input = parse raw
        com = buildMass input "COM"
    print $ part1 com
    print $ part2 com
