import System.IO
import Data.List
import Data.Maybe
import Debug.Trace

data Thing = Thing Int String deriving (Show,Eq)

-- Items required to produce Thing
data Reaction = Reaction Thing [Thing] deriving Show

-- Things that require this element
data Need = Need String [String] deriving Show

data Input = Input [Reaction] [Need]

parseReaction :: [String] -> Reaction--([Thing], Thing)
parseReaction ["=>", a, b] = Reaction (Thing (read a) b) []
parseReaction (x:y:xs) = let
        v = Thing (read x) y
        (Reaction o vs) = parseReaction xs
    in Reaction o (v:vs)

parse = map (parseReaction.words) . lines . (filter (/= ','))

buildInput r = let
        oreNeed = buildNeeds r "ORE"
        otherNeed = map (buildNeeds r . rName) r
    in Input r (oreNeed:otherNeed)

-- Return rn if if needs name
ifNeeds :: String -> Reaction -> Maybe String
ifNeeds name (Reaction (Thing _ rn) i) =
    case find (\(Thing _ tn) -> tn == name) i of
        Nothing -> Nothing
        _ -> Just rn

buildNeeds :: [Reaction] -> String -> Need
buildNeeds r name = Need name (mapMaybe (ifNeeds name) r)

needReady :: Need -> Bool
needReady (Need n []) = True
needReady _ = False

removeNeed :: String -> Need -> Maybe Need
removeNeed name (Need n ns)
    | n == name = Nothing
    | otherwise = Just (Need n (filter (/= name) ns))

rName :: Reaction -> String
rName (Reaction (Thing _ tn) _) = tn

satisfy :: [Reaction] -> [Need] -> [Thing] -> Int
satisfy r nd de = let
        (Just (Need ready _)) = find needReady nd
        nd' = mapMaybe (removeNeed ready) nd
        (rde, de') = partition (\(Thing _ tn) -> tn == ready) de
        c = sum (map (\(Thing tc _) -> tc) rde)
    in if ready == "ORE" then c
    else let
        (Just (Reaction (Thing i _) o)) = find (\r -> rName r == ready) r
        nr = (c + i - 1) `div` i
        newDe = de' ++ (map (\(Thing tc tn) -> Thing (tc * nr) tn) o)
    in satisfy r nd' newDe

oreNeeded (Input r nd) n = 
    satisfy r nd [Thing n "FUEL"]

part1 inp = oreNeeded inp 1

main = do
    raw <- readFile "input"
    let
        r = parse raw
        inp = buildInput r
    print $ part1 inp
