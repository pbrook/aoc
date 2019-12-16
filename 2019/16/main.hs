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

applyN :: (a -> a) -> Int -> a -> a
applyN f 0 x = x
applyN f n x = applyN f (n-1) (f x)

part1 inp = applyN fft 100 inp

fftR :: [Int] -> [Int]
fftR (x:xs) = map (`mod` 10) (scanl' (+) x xs)

-- Notice that the last half of the fft is the sum of the subsequent elements
part2 inp = let
        len = 10000 * length inp
        n = read (take 7 (map intToDigit inp)) :: Int
    in case n > (len `div` 2) of
        False -> undefined
        True -> let
                ri = reverse inp
                t = take (len - n) (cycle ri)
            in reverse (applyN fftR 100 t)

main = do
    raw <- readFile "input"
    let
        inp = parse raw
    putStrLn $ top8 (part1 inp)
    putStrLn $ top8 (part2 inp)
