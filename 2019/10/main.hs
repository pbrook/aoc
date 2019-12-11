import System.IO
import Data.Maybe
import Data.List
import Data.Ratio

data Point = Point Int Int deriving Show
data Vector = Vector Int Int Int deriving Show

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

getVector :: Point -> Point -> Maybe Vector
getVector (Point ax ay) (Point bx by) = let
        x = bx - ax
        y = by - ay
    in if x == 0 && y == 0
        then Nothing
        else let d = gcd x y in Just (Vector (x `div` d) (y `div` d) d)

-- We don't actually need an accurate angle, a (cheaper) slope value would be
-- sufficient.  However atan2 already knows how to map quadrants correctly,
-- so for simplicitly we use that
-- (atan2 y x) gives the angle from the x-axis.
-- We want clockwise from the y-axis, so (atan2 -x y)
angle :: Vector -> Float
angle (Vector x y _)
    -- Special case the ambiguous boundary case to make sure it is at the start
    | x == 0 && y < 0 = -pi
    | otherwise = atan2 (-fromIntegral x) (fromIntegral y)

compareVector :: Vector -> Vector -> Ordering
compareVector a@(Vector _ _ ad) b@(Vector _ _ bd)
    | sameDirection a b = ad `compare` bd
    | otherwise = (angle a) `compare` (angle b)

sameDirection :: Vector -> Vector -> Bool
sameDirection (Vector ax ay _) (Vector bx by _) = (ax == bx) && (ay == by)

getVectors :: [Point] -> Point -> (Int, [Vector], Point)
getVectors others me = let
        v = mapMaybe (getVector me) others
        v' = sortBy compareVector v
        (a, b) = sweep v'
    in (length a, a ++ sweepAll b, me)

bestVector :: (Int, [Vector], Point) -> (Int, [Vector], Point) -> (Int, [Vector], Point)
bestVector (an, av, ap) (bn, bv, bp)
    | an > bn = (an, av, ap)
    | otherwise = (bn, bv, bp)

bestStation :: [Point] -> (Int, [Vector], Point)
bestStation a = let
        v = map (getVectors a) a
    in foldl1 bestVector v


sweep :: [Vector] -> ([Vector], [Vector])
sweep [] = ([], [])
sweep [x] = ([x], [])
sweep (x:y:ys)
    | sameDirection x y = let
            (a, b) = sweep (x:ys)
        in (a, y:b)
    | otherwise = let
            (a, b) = sweep (y:ys)
        in (x:a, b)

sweepAll :: [Vector] -> [Vector]
sweepAll x = let
        (a, b) = sweep x
    in a ++ case b of
        [] -> []
        _ -> sweepAll b

part2 v (Point px py) = let
        v' = sweepAll v
        (Vector x y d) = v' !! 199
        x' = x * d + px
        y' = y * d + py
    in x' * 100 + y'

main = do
    raw <- readFile "input"
    let input = parse raw
        (n, v, p) = bestStation input
    print $ n
    print $ part2 v p
