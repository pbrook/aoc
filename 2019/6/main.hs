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

totalOrbits m@(Mass name s) = numSat m + sum (map totalOrbits s)

part1 input = let
        com = buildMass input "COM"
    in totalOrbits com

main = do
    raw <- readFile "input"
    let input = parse raw
    print $ part1 input
