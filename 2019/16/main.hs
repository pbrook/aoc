-- Part2 assumes a 64-bit Int
import System.IO
import Data.List
import Data.Char
import Debug.Trace

parse = map digitToInt . filter isDigit

top8 = map intToDigit . take 8

pattern :: Int -> [Int]
pattern n = let
        base = map (replicate n) [0, 1, 0, -1]
        p = cycle (concat base)
    in tail p

fftDigit :: Int -> [Int] -> Int
fftDigit n = (`mod` 10) . abs . sum . zipWith (*) (pattern n)

fft :: [Int] -> [Int]
fft x = [fftDigit n x | n <- [1..(length x)]]

fftN :: Int -> [Int] -> [Int]
fftN 0 x = x
fftN n x = fftN (n-1) (fft x)

part1 inp = fftN 100 inp

main = do
    raw <- readFile "input"
    let
        inp = parse raw
    putStrLn $ top8 (part1 inp)
