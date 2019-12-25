import Data.Array
import Data.List
import Data.Char
import Data.Maybe

import Debug.Trace

data Point = Point Int Int deriving (Show,Eq)
data Tile = Tile Point Char deriving Show
type Grid = [Tile]

parseLine :: Point -> String -> [Tile]
parseLine _ [] = []
parseLine p@(Point x y) (c:cs) = let
        rest = parseLine (Point (x+1) y) cs
    in case c of
    '#' -> rest
    a -> (Tile p a):rest

parseGrid :: Int -> [String] -> [Tile]
parseGrid _ [] = []
parseGrid y (r:rs) = (parseLine (Point 0 y) r) ++ (parseGrid (y+1) rs)

parse :: String -> [Tile]
parse inp = parseGrid 0 (lines inp)

{-
seenTile :: [Tile] -> Point -> Bool
seenTile ts p@(Point x y) = any (\(Tile tp _) -> p == tp) ts

searchStep :: Machine -> Point -> [Tile] -> Int -> [Tile]
searchStep m (Point x y) ts dir = let
        p = case dir of
            1 -> Point x (y-1)
            2 -> Point x (y+1)
            3 -> Point (x-1) y
            4 -> Point (x+1) y
    in if seenTile ts p then ts
    else let
        (m', Output result) = step (pushInput m dir)
    --in case trace (show (result, dir, p)) result of
    in case result of
        0 -> (Tile p 1):ts
        1 -> search m' p ((Tile p 0):ts)
        2 -> (Tile p 2):ts

search :: Machine -> Point -> [Tile] -> [Tile]
search m p ts
    -- | trace (show p) False = undefined
    | otherwise = let
        x1 = searchStep m p ts 1
        x2 = searchStep m p x1 2
        x3 = searchStep m p x2 3
        x4 = searchStep m p x3 4
    in x4

nextTo :: Point -> Point -> Bool
nextTo (Point px py) (Point tx ty)
    | px /= tx && py /= ty = False
    | px == tx + 1 || px == tx - 1 = True
    | py == ty + 1 || py == ty - 1 = True
    | otherwise = False

isOrigin :: [Point] -> Bool
isOrigin = any (== (Point 0 0))

walk :: ([Point] -> Bool) -> [Point] -> Int -> [Point] -> Int
walk cond path steps p = let
        (near, far) = partition (nearAny p) path
    in case cond near of
        True -> steps
        False -> walk cond far (steps+1) near
-}

nextTo :: Tile -> Tile -> Bool
nextTo (Tile (Point ax ay) _) (Tile (Point bx by) _)
    | ax /= bx && ay /= by = False
    | ax == bx + 1 || ax == bx - 1 = True
    | ay == by + 1 || ay == by - 1 = True
    | otherwise = False

nearAny :: [Tile] -> Tile -> Bool
nearAny ps t = any (nextTo t) ps

isInteresting :: Tile -> Bool
isInteresting t@(Tile _ c) = c /= '.'

data Dest = Dest Char Int deriving Show
data Mesh = Mesh Char [Dest] deriving Show

walk :: Grid -> Int -> [Tile] -> [Dest]
walk _ _ [] = []
walk g steps t = let
        (near, far) = partition (nearAny t) g
        (k, t') = partition isInteresting near
        mesh = [Dest c steps | Tile _ c <- k]
    in mesh ++ walk far (steps + 1) t'

makeMesh :: [Tile] -> Tile -> Mesh
makeMesh g t@(Tile _ c) = let
        d = walk g 1 [t]
        d' = filter (\(Dest a _) -> a /= c) d
    in Mesh c d'


isDest :: Char -> Dest -> Bool
isDest c (Dest x _) = c == x

mergeDistance :: Int -> [Dest] -> Dest -> [Dest]
mergeDistance extra d (Dest c steps) = let
        (this, others) = partition (isDest c) d
        newSteps = steps + extra
        best = case this of
            [] -> newSteps
            [Dest _ oldSteps] -> min newSteps oldSteps
    in (Dest c best):others

shortcutNode :: Mesh -> Mesh -> Mesh
shortcutNode me@(Mesh c d) m@(Mesh mc md) = let
        (x, md') = partition (isDest c) md
    in case x of
        [] -> m
        [Dest _ extra] -> let
                d' = filter (not.isDest mc) d
                md'' = foldl' (mergeDistance extra) md' d'
            in Mesh mc md''

removeNode :: [Mesh] -> Char -> (Maybe Mesh, [Mesh])
removeNode mesh c = let
        (me', m') = partition (\(Mesh x _) -> x == c) mesh
    in case me' of
        [] -> (Nothing, m')
        [me] -> (Just me, map (shortcutNode me) m')

unlock mesh '@' = mesh
unlock mesh c = snd $ removeNode mesh c

tryKeys ch best _ _ [] = best
tryKeys ch best steps mesh ((Dest kc ks):k)
    | best <= steps + ks = best
    | otherwise = let
            this = pickup ch best (steps + ks) mesh kc
            best' = min this best
        in tryKeys ch best' steps mesh k

pickup ch best steps mesh c
    -- | trace ("Pickup " ++ [c] ++ (show steps)) False = undefined
    | otherwise = let
        m' = unlock mesh (toUpper c)
        (Just me@(Mesh _ d), newMesh) = removeNode m' c
        k = filter (\(Dest x _) -> isLower x) d
    in case k of
        [] -> trace ((show steps)++" "++(show best)++ch) steps
        _ -> tryKeys (c:ch) best steps newMesh k

part1 g = let
        k = filter isInteresting g
        mesh = map (makeMesh g) k
    in pickup "" 9999 0 mesh '@'

main = do
    raw <- readFile "input"
    let input = parse raw
    print $ part1 input
