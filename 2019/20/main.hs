import Data.Array
import Data.List
import Data.Char
import Data.Maybe

import Debug.Trace

data Point = Point Int Int deriving (Show,Eq)
data Edge = Edge Point Point deriving Show
data Portal = Portal String Point deriving Show


parseLine :: Point -> String -> [Edge]
parseLine _ (_:[]) = []
parseLine p@(Point x y) (c:cn:cs) = let
        next = Point (x+1) y
        rest = parseLine next (cn:cs)
    in case c:cn:[] of
    ".." -> (Edge p next):rest
    _ -> rest

parseEdges :: Int -> [String] -> [Edge]
parseEdges _ [] = []
parseEdges y (r:rs) = (parseLine (Point 0 y) r) ++ (parseEdges (y+1) rs)

transposePoint (Point x y) = Point y x

transposeEdge (Edge p1 p2) = Edge (transposePoint p1) (transposePoint p2)

portalAt :: Int -> String -> (Int, Int) -> Maybe Portal
portalAt y s (o,x) = let
        name = take 2 (drop o s)
    in case name of
        '.':_ -> Nothing
        '#':_ -> Nothing
        ' ':_ -> Nothing
        [_, ' '] -> Nothing
        _ -> Just (Portal name (Point x y))

findPortals :: [(Int, Int)] -> Int -> [String] -> [Portal]
findPortals _ _ [] = []
findPortals cols y (s:ss) = let
        p = catMaybes $ map (portalAt y s) cols
    in p ++ findPortals cols (y+1) ss

transposePortal (Portal name p) = Portal name (transposePoint p)

findPortal name ps = let
        ([Portal _ p], rest) = partition (\(Portal pn _) -> pn == name) ps
    in (p, rest)

matchPortals :: [Portal] -> [Edge]
matchPortals [] = []
matchPortals ((Portal name p1):ps) = let
        (p2, rest) = findPortal name ps
    in (Edge p1 p2):(matchPortals rest)
    
parsePortal' :: [String] -> [Portal]
parsePortal' maze = let
        ll = length (head maze)
        mid = (length maze) `div` 2
        ml = maze !! mid
        (Just w) = findIndex (\c -> c /= '#' && c /= '.') (drop 2 ml)
        cols = [(0, 2), (w+2,w+1), (ll-(w+4),ll-(w+2)), (ll-2,ll-3)]
        p = findPortals cols 0 maze
    in p


parsePortals :: [String] -> (Point, Point, [Edge])
parsePortals maze = let
        ph = parsePortal' maze
        pv = map transposePortal (parsePortal' (transpose maze))
        (start, p) = findPortal "AA" (ph ++ pv)
        (end, p') = findPortal "ZZ" p
        pe = matchPortals p'
    in (start, end, pe)

partitionEdges :: Point -> [Edge] -> ([Point], [Edge])
partitionEdges _ [] = ([], [])
partitionEdges p ((e@(Edge p1 p2)):es)
    | p1 == p = (p2:rp, re)
    | p2 == p = (p1:rp, re)
    | otherwise = (rp, e:re)
    where (rp, re) = partitionEdges p es
    
walkEdges :: [Point] -> [Edge] -> ([Point], [Edge])
walkEdges [] e = ([], e)
walkEdges _ [] = ([], [])
walkEdges (p:ps) e = let
        (p', e') = partitionEdges p e
        (rp, re) = walkEdges ps e'
    in (p' ++ rp, re)

walk :: Int -> Point -> [Point] -> [Edge] -> Int
walk steps end p e 
    -- | trace (show p) False = undefined
    | otherwise = let
            (p', e') = walkEdges p e
        in if any (== end) p'
        then steps
        else walk (steps+1) end p' e'

part1 inp = let
        l = lines inp
        he = parseEdges 0 l
        ve = map transposeEdge (parseEdges 0 (transpose l))
        (start, end, pe) = parsePortals l
        e = he ++ ve ++ pe
    in walk 1 end [start] e

main = do
    raw <- readFile "input"
    --let input = parse raw
    print $ part1 raw
