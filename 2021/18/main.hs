import System.IO
import System.Exit
import Data.Char
import Debug.Trace
import Data.List

-- Node depth value
data Node = Node Int Int deriving (Show, Eq)

parse1 :: Int -> Int -> String -> [Node]
parse1 d v (c:xs)
    | c == '[' = parse1 (d+1) v xs
    | c == ']' = parse1 (d-1) v xs
    | c == ',' = (Node d v):parse1 0 0 xs
    | otherwise = parse1 d (digitToInt c) xs
parse1 d v [] = [Node d v]

parse = map (parse1 0 0) . lines

push_right _ [] = []
push_right addval ((Node d v):xn) = (Node d (v + addval)):xn

explode1 :: Int -> Node -> [Node] -> [Node]
explode1 _ prev [] = [prev]
explode1 d0 prev (n@(Node d v):xn)
    | newdepth <= 4 = prev:explode1 newdepth n xn
    | otherwise = let 
            Node pd pv = prev
            Node nd nv = head xn
            thisd = if d == 1 then nd  + 1 else d - 1
            this = Node thisd 0
        in
            (Node pd (pv + v)):this:push_right nv (tail xn)
    where newdepth = d0 + d

explode val = tail $ explode1 0 (Node 0 0) val

split [] = []
split (n@(Node d v):xn)
    | v < 10 = n:split xn
    | otherwise = let
            left = v `div` 2
            right = v - left
            (dl, dr) = if d > 0 then (d+1, -1) else (1, d-1)
        in (Node dl left):(Node dr right):xn

debug x = trace (dump x) x

reduce val
    | val /= e_val = reduce e_val
    | val /= s_val = reduce s_val
    | otherwise = val
    where
        e_val = explode val
        s_val = split val

declast [Node d v] = [Node (d-1) v]
declast (a:xs) = a:declast xs

add :: [Node] -> [Node] -> [Node]
add ((Node d n):xa) b =
    let val = (Node (d+1) n):xa ++ declast b
        in reduce val

mag1 (Node d v) rest
    | d <= 0 = (v, rest)
    | otherwise = let (rv, rtail) = mag1 (head rest) (tail rest)
        in mag1 (Node (d-1) (v * 3 + rv * 2)) rtail

magnitude (n:ns) = fst $ mag1 n ns

printNode :: Node -> String
printNode (Node d n)
    | d > 0 = (replicate d '[') ++ (show n)
    | d < 0 = (show n) ++ (replicate (-d) ']')

dump :: [Node] -> String
dump = (intercalate ",") . map printNode

addpairs list n = map (magnitude . (add n)) $ filter (/= n) list

part1 = magnitude . foldl1' add
part2 list = maximum [magnitude $ add a b | a <- list, b <- list, a /= b]

assert x y = do
    if x /= y
        then die $ "FAIL: Expected:" ++ (show y) ++ " got:" ++ (show x)
        else return ()

test file p1 p2 = do
    raw <- readFile file
    let input = parse raw
    assert (part1 input) p1
    case p2 of
        Just n -> assert (part2 input) n
        _ -> return ()

main = do
    test "test0" 1384 Nothing
    test "test1" 3488 Nothing
    test "test2" 4140 (Just 3993)

    raw <- readFile "input"
    let input = parse raw
    print $ part1 input
    print $ part2 input
