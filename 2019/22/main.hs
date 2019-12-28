import Data.Array
import Data.List
import Data.Char
import Data.Maybe

import Debug.Trace

data Shuffle = Reverse | Cut Int | Increment Int deriving Show

packSize = 10007

parseLine s = case head s of
    'c' -> let n = read (drop 4 s)
        in Cut (if n > 0 then n else packSize + n)
    _ -> case s !! 5 of
        'i' -> Reverse
        _ -> Increment (read (drop 20 s))

parse = map parseLine . lines 

mapShuffle n Reverse = packSize - (n + 1)
mapShuffle n (Cut x) = if n < x then packSize + n - x else n - x
mapShuffle n (Increment x) = (n * x) `mod` packSize

{-
factoryDeck = [0..(packSize - 1)]
part1Test s = let 
        m = map (\x -> foldl' mapShuffle x s) factoryDeck
    in [fromJust (findIndex (==x) m) | x <- factoryDeck]
-}

part1 = foldl' mapShuffle 2019

main = do
    raw <- readFile "input"
    let input = parse raw
    print $ part1 input
