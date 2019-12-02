import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Array

type Memory = Array Int Int

parse :: T.Text -> [Int]
parse = (map (read . T.unpack)) . (T.splitOn (T.singleton ','))

writeback :: Memory -> Int -> (Int -> Int -> Int) -> Int
writeback mem pc fn = let
        src1 = mem ! (mem ! (pc + 1))
        src2 = mem ! (mem ! (pc + 2))
        val = fn src1 src2
        dest = mem ! (pc + 3)
        newmem = mem // [(dest, val)]
    in run newmem (pc + 4)

run :: Memory -> Int -> Int
run mem pc = let
        op = mem ! pc
    in case op of
        1 -> writeback mem pc (+)
        2 -> writeback mem pc (*)
        99 -> mem ! 0

patchRun :: Memory -> Int -> Int -> Int
patchRun mem noun verb = let
        patched = mem // [(1, noun), (2, verb)]
    in run patched 0

part1 mem = patchRun mem 12 2

part2 mem result = head [100 * noun + verb | noun <- indices mem, verb <- indices mem, patchRun mem noun verb == result]

main = do
    raw <- T.IO.readFile "input"
    let input = parse raw
        mem = listArray (0, (length input) - 1) input
    print $ part1 mem
    print $ part2 mem 19690720
