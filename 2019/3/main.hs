import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

-- Could have separate up/down/left/right instances, but not worth the pain
data Direction = Direction Char Int
-- Point holds x, y and wire length
data Point = Point Int Int Int deriving Show
-- Segment is a starting point and a length (may be negative)
data Segment = Vertical Point Int | Horizontal Point Int deriving Show


parseDirection :: T.Text -> Direction
parseDirection x = Direction (T.head x) ((read . T.unpack . T.tail) x)

parseLine :: T.Text -> [Direction]
parseLine = (map parseDirection) . (T.splitOn (T.singleton ','))

parse :: T.Text -> [[Direction]]
parse = (map parseLine) . T.lines

makeSegments :: Point -> [Direction] -> [Segment]
makeSegments _ [] = []
makeSegments start@(Point x y d) ((Direction p w):rest)
    | p == 'U' = (Vertical start (-w))
        :makeSegments (Point x (y - w) d') rest
    | p == 'D' = (Vertical start w)
        :makeSegments (Point x (y + w) d') rest
    | p == 'L' = (Horizontal start (-w))
        :makeSegments (Point (x - w) y d') rest
    | p == 'R' = (Horizontal start w)
        :makeSegments (Point (x + w) y d') rest
    where
        d' = d + w

-- We assume we can ignore collinear segments
-- Our overlapping segment ends will hide this bug in many cases
-- We could fix this by returning [Point] and using concatMap
-- instead of comprehension pattern matching in intersectList
intersectOne :: Segment -> Segment -> Maybe Point
intersectOne (Vertical _ _) (Vertical _ _) = Nothing
intersectOne (Horizontal _ _) (Horizontal _ _) = Nothing
intersectOne a@(Horizontal _ _) b@(Vertical _ _) = intersectOne b a
intersectOne (Vertical (Point x1 y1 d1) w1) (Horizontal (Point x2 y2 d2) w2)
    | x1 < left = Nothing
    | x1 > right = Nothing
    | y2 < top = Nothing
    | y2 > bottom = Nothing
    | otherwise = Just (Point x1 y2 (d1 + d2 + abs (y1 - y2) + abs (x2 - x1)))
    where
        top = min y1 (y1 + w1)
        bottom = max y1 (y1 + w1)
        left = min x2 (x2 + w2)
        right = max x2 (x2 + w2)


intersectList :: [Segment] -> Segment -> [Point]
intersectList x a = [p | Just p <- map (intersectOne a) x]

intersect :: [[Segment]] -> [Point]
intersect [a, b] = concatMap (intersectList a) b

manhattan :: Point -> Int
manhattan (Point x y d) = (abs x) + (abs y)

findBest :: (Point -> Int) -> [[Direction]] -> Int
findBest distFn input = let
        segments = map (makeSegments (Point 0 0 0)) input
        points = intersect segments
        distance = map distFn points
    in foldr1 min (filter (/= 0) distance)

part1 = findBest manhattan

wireLength :: Point -> Int
wireLength (Point x y d) = d

part2 = findBest wireLength

main = do
    raw <- T.IO.readFile "input"
    let input = parse raw
    print $ part1 input
    print $ part2 input
