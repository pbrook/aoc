-- Part1 only solution
-- Kept for interest because like being able to ignore segment direction
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

-- Could have separate up/down/left/right instances, but not worth the pain
data Direction = Direction Char Int
data Point = Point Int Int deriving Show
-- The origin of a segment is the top/left end
-- The end of one segment will overlap with the start of the next
-- This allows us to ignore segment direction
data Segment = Vertical Point Int | Horizontal Point Int deriving Show


parseDirection :: T.Text -> Direction
parseDirection x = Direction (T.head x) ((read . T.unpack . T.tail) x)

parseLine :: T.Text -> [Direction]
parseLine = (map parseDirection) . (T.splitOn (T.singleton ','))

parse :: T.Text -> [[Direction]]
parse = (map parseLine) . T.lines

makeSegments :: Int -> Int -> [Direction] -> [Segment]
makeSegments _ _ [] = []
makeSegments x y ((Direction 'U' w):rest) =
    (Vertical (Point x (y - w)) w)
    :makeSegments x (y - w) rest
makeSegments x y ((Direction 'D' w):rest) =
    (Vertical (Point x y) w)
    :makeSegments x (y + w) rest
makeSegments x y ((Direction 'L' w):rest) =
    (Horizontal (Point (x - w) y) w )
    :makeSegments (x - w) y rest
makeSegments x y ((Direction 'R' w):rest) =
    (Horizontal (Point x y) w)
    :makeSegments (x + w) y rest

intersectOne :: Segment -> Segment -> Maybe Point
-- We assume we can ignore collinear segments
-- Our overlapping segment ends will hide this bug in many cases
intersectOne (Vertical _ _) (Vertical _ _) = Nothing
intersectOne (Horizontal _ _) (Horizontal _ _) = Nothing
intersectOne a@(Horizontal _ _) b@(Vertical _ _) = intersectOne b a
intersectOne (Vertical (Point x1 y1) w1) (Horizontal (Point x2 y2) w2)
    | x1 < x2 = Nothing
    | x1 > (x2 + w2) = Nothing
    | y2 < y1 = Nothing
    | y2 > (y1 + w1) = Nothing
    | otherwise = Just (Point x1 y2)


intersectList :: [Segment] -> Segment -> [Point]
intersectList x a = [p | Just p <- map (intersectOne a) x]

intersect :: [[Segment]] -> [Point]
intersect [a, b] = concatMap (intersectList a) b

manhattan :: Point -> Int
manhattan (Point x y) = (abs x) + (abs y)

validDistance :: [Point] -> [Int]
validDistance = filter (/= 0) . map manhattan

part1 input = let
        segments = map (makeSegments 0 0) input
        points = intersect segments
        distance = validDistance points
    in foldr1 min distance

main = do
    raw <- T.IO.readFile "input"
    let input = parse raw
    print $ part1 input
