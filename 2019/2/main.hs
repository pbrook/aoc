import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Array

parse :: T.Text -> [Int]
parse = (map (read . T.unpack)) . (T.splitOn (T.singleton ','))

writeback :: Array Int Int -> Int -> (Int -> Int -> Int) -> Int
writeback mem pc fn = let
        src1 = mem ! (mem ! (pc + 1))
        src2 = mem ! (mem ! (pc + 2))
        val = fn src1 src2
        dest = mem ! (pc + 3)
        newmem = mem // [(dest, val)]
    in run newmem (pc + 4)

run :: Array Int Int -> Int -> Int
run mem pc = let
        op = mem ! pc
    in case op of
        1 -> writeback mem pc (+)
        2 -> writeback mem pc (*)
        99 -> mem ! 0

part1 input = let
        mem = listArray (0, (length input) - 1) input
        corrupted = mem // [(1, 12), (2, 2)]
    in run corrupted 0

main = do
    raw <- T.IO.readFile "input"
    let input = parse raw
    print $ part1 input
    --print $ part2 input
