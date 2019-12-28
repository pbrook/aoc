import Data.Array
import Data.List
import Data.Char
import Data.Maybe

import Debug.Trace

data Point = Point Int Int deriving (Show,Eq)
data Edge = Edge Point Point deriving Show
data Portal = Portal String Point Bool deriving Show


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

portalAt :: Int -> String -> (Int, Int, Bool) -> Maybe Portal
portalAt y s (o,x,up) = let
        name = take 2 (drop o s)
    in case name of
        '.':_ -> Nothing
        '#':_ -> Nothing
        ' ':_ -> Nothing
        [_, ' '] -> Nothing
        _ -> Just (Portal name (Point x y) up)

findPortals :: [(Int, Int, Bool)] -> Int -> [String] -> [Portal]
findPortals _ _ [] = []
findPortals cols y (s:ss) = let
        p = catMaybes $ map (portalAt y s) cols
    in p ++ findPortals cols (y+1) ss

transposePortal (Portal name p d) = Portal name (transposePoint p) d

findPortal name ps = let
        ([Portal _ p _], rest) = partition (\(Portal pn _ _) -> pn == name) ps
    in (p, rest)

matchPortals :: [Portal] -> [Edge]
matchPortals [] = []
matchPortals ((Portal name p1 _):ps) = let
        (p2, rest) = findPortal name ps
    in (Edge p1 p2):(matchPortals rest)
    
parsePortal' :: [String] -> [Portal]
parsePortal' maze = let
        ll = length (head maze)
        mid = (length maze) `div` 2
        ml = maze !! mid
        (Just w) = findIndex (\c -> c /= '#' && c /= '.') (drop 2 ml)
        cols = [(0, 2, True), (w+2,w+1,False), (ll-(w+4),ll-(w+2),False), (ll-2,ll-3,True)]
        p = findPortals cols 0 maze
    in p

parsePortals :: [String] -> (Point, Point, [Portal])
parsePortals maze = let
        ph = parsePortal' maze
        pv = map transposePortal (parsePortal' (transpose maze))
        (start, p) = findPortal "AA" (ph ++ pv)
        (end, p') = findPortal "ZZ" p
    in (start, end, p')

parse inp = let
        l = lines inp
        he = parseEdges 0 l
        ve = map transposeEdge (parseEdges 0 (transpose l))
        (start, end, p) = parsePortals l
    in (start, end, p, he ++ ve)

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
walk steps end p e = let
            (p', e') = walkEdges p e
        in if any (== end) p'
        then steps
        else walk (steps+1) end p' e'

part1 (start, end, p, e) = let
        pe = matchPortals p
    in walk 1 end [start] (e ++ pe)

floodFrom :: Int -> [Point] -> [Edge] -> [(Point, Int)]
floodFrom _ [] _ = []
floodFrom _ _ [] = []
floodFrom steps p e = let
        (p', e') = walkEdges p e
    in (map (\x -> (x, steps)) p') ++ (floodFrom (steps+1) p' e')

data Gateway = Gateway String Bool deriving (Eq,Show)

floodDistance :: [(Point, Int)] -> Portal -> Maybe (String, Bool, Int)
floodDistance f (Portal n p up) = case lookup p f of
    Nothing -> Nothing
    Just d -> Just (n, up, d+1)

type Mesh = (Gateway, [(String, Bool, Int)])
type MeshPos = (Gateway, Int, Int)

floodPortal :: [Portal] -> [Edge] -> Portal -> Mesh
floodPortal ports e start@(Portal sn sp up) = let
        f = floodFrom 1 [sp] e
        dm = mapMaybe (floodDistance f) ports
    in ((Gateway sn up), dm)

stepMesh :: [Mesh] -> MeshPos -> [MeshPos]
stepMesh mesh (gw,level,steps) = let
        next = fromJust $ lookup gw mesh
    in [((Gateway nn (not up)), nl, steps+nd) |
        (nn, up, nd) <- next,
        let nl = if up then level-1 else level+1,
        nl >= 0]

findExit :: [(Gateway, Int)] -> MeshPos -> Maybe Int
findExit epos (gw, level, steps) = if level /= 0
    then Nothing
    else case lookup gw epos of
        Nothing -> Nothing
        Just d -> Just (steps + d - 1)

walkMesh best _ _ [] = best
walkMesh best epos mesh start
    -- | trace ("Walk:"++(show best)++show start) False = undefined
    | otherwise = let
        next = concat $ map (stepMesh mesh) start
        exits = best:(mapMaybe (findExit epos) next)
        best' = minimum exits
        next' = filter (\(gw,level,steps) -> steps < best') next
    in walkMesh best' epos mesh next'

part2 (start, end, ports, e) = let
        mesh = map (floodPortal ports e) ports
        (_,sm) = floodPortal ports e (Portal "AA" start True)
        (_,em) = floodPortal ports e (Portal "ZZ" end True)
        pos = [((Gateway n True),1,d)|(n,up,d) <- sm, up == False]
        epos = [((Gateway n False),d)|(n,up,d) <- em, up == False]
    in walkMesh 9999 epos mesh pos

main = do
    raw <- readFile "input"
    let input = parse raw
    print $ part1 input
    print $ part2 input
