import System.IO
import Data.Maybe
import Data.List
import Data.Ratio

data Point = Point Int Int deriving Show
data Direction = Direction Int Int deriving (Show, Eq)

parseLine :: (Int, Int) -> String -> [Maybe Point]
parseLine _ [] = []
parseLine (x, y) (c:cs) = case c of
        '#' -> Just (Point x y)
        _ -> Nothing
    :parseLine (x+1, y) cs

parseGrid :: Int -> String -> (Int, [Point])
parseGrid y s = (y+1, catMaybes (parseLine (0, y) s))

parse :: String -> [Point]
parse x = let
        (_, p) = mapAccumL parseGrid 0 (lines x)
    in concat p

getDirection :: Point -> Point -> Direction
getDirection (Point ax ay) (Point bx by) = let
        x = ax - bx
        y = ay - by
    in if x == 0 && y == 0
        then Direction 0 0
        else let d = gcd x y in Direction (x `div` d) (y `div` d)

countVisible :: [Point] -> Point -> Int
countVisible others me = let
        vectors = map (getDirection me) others
    in length (nub vectors)

part1 a = maximum (map (countVisible a) a) - 1

main = do
    raw <- readFile "input"
    let input = parse raw
    print $ part1 input
