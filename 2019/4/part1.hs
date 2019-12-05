import Data.List

triangle :: Int -> [Int] -> Int -> Int
triangle 0 _ _ = 1
triangle 1 (limit:rest) start
    | limit >= start = limit + 1 - start
    | otherwise = 0

triangle n (limit:rest) start
    | limit >= start = sum (map (triangle (n - 1) nines) [start..limit - 1]) + triangle (n-1) rest limit
    | otherwise = 0

nines = [9,9,9,9,9,9,9]

pairs :: Int -> [Int] -> Int -> Int
pairs 1 _ start = 0

pairs n (limit:rest) start
 | limit < start = 0
 | otherwise = 
    sum (map (\x -> triangle (n-2) nines x + pairs (n-1) nines (x+1)) [start .. limit-1]) + 
    (case limit `compare` (head rest) of
        EQ -> triangle (n-2) (tail rest) limit
        LT -> triangle (n-2) nines limit
        GT -> 0
    ) + 
    pairs (n-1) rest (limit+1)

main = do
    let input = (246515, 739105)
    let a = pairs 6 [2,4,6,5,1,5] 0
        b = pairs 6 [7,3,9,1,0,5] 0
    print (b - a)
