import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Array

type Memory = Array Int Int

data Machine = Machine {
    mPC :: Int,
    mMemory :: Memory,
    mInput :: [Int],
    mOutput :: [Int],
    mTrace :: [MachineOp]}
data MachineOp = IncPC Int
    | Jump Int
    | MemWrite Int Int
    | PopInput
    | PushOutput Int
    deriving Show

parse :: T.Text -> [Int]
parse = (map (read . T.unpack)) . (T.splitOn (T.singleton ','))

writeback :: Machine -> MachineOp -> Machine
writeback m@(Machine{mPC = pc}) (IncPC n) = m {mPC = pc + n}
writeback m@(Machine{mPC = pc}) (Jump x) = m {mPC = x}
writeback m@(Machine{mMemory = mem}) (MemWrite dest val) = m {mMemory = mem // [(dest, val)]}
writeback m@(Machine{mInput=inp}) PopInput = m {mInput = tail inp}
writeback m@(Machine{mOutput=out}) (PushOutput val) = m {mOutput = val:out}

trace m@(Machine{mTrace=log}) op = writeback (m {mTrace=op:log}) op

argType :: Int -> Int -> Int
argType op 1 = (op `div` 100) `mod` 10
argType op 2 = (op `div` 1000) `mod` 10
    
getArg :: Machine -> Int -> Int
getArg (Machine {mMemory=mem, mPC=pc}) n = let
        op = mem ! pc
        imm = mem ! (pc + n)
    in case argType op n of
        0 -> mem ! imm
        1 -> imm

opWrite :: Machine -> Int -> Int -> MachineOp
opWrite (Machine {mPC=pc, mMemory=mem}) n val = let
        addr = mem ! (pc + n)
    in MemWrite addr val

opBinary :: Machine -> (Int -> Int -> Int) -> [MachineOp]
opBinary m fn = let
        src1 = getArg m 1
        src2 = getArg m 2
        val = fn src1 src2
    in [opWrite m 3 val, (IncPC 4)]

opInput :: Machine -> [MachineOp]
opInput m@(Machine {mInput=input}) =
    [(opWrite m 1 (head input)), PopInput, (IncPC 2)]

opOutput :: Machine -> [MachineOp]
opOutput m@(Machine {mInput=input}) = let
        val = getArg m 1
    in [(PushOutput val), (IncPC 2)]

opJump :: Machine -> (Int -> Bool) -> [MachineOp]
opJump m cond = let
        val = getArg m 1
        addr = getArg m 2
    in if cond val
    then [Jump addr]
    else [IncPC 3]

run :: Machine -> Machine
run m@(Machine {mPC=pc, mMemory=mem}) = let
        op = mem ! pc
        mop = case op `mod` 100 of
            1 -> opBinary m (+)
            2 -> opBinary m (*)
            3 -> opInput m
            4 -> opOutput m
            5 -> opJump m (/=0)
            6 -> opJump m (==0)
            7 -> opBinary m (\x y -> if x < y then 1 else 0)
            8 -> opBinary m (\x y -> if x == y then 1 else 0)
            _ -> []
    in case mop of
        [] -> m
        _ -> run (foldl trace m mop)

runMachine :: Memory -> [Int] -> Machine
runMachine mem inp = let
        start = Machine {mMemory=mem, mPC=0, mInput=inp, mOutput=[], mTrace=[]}
        end = run start
    in end

part1 mem = mOutput (runMachine mem [1])

part2 mem = mOutput (runMachine mem [5])
--part2 mem = reverse $ mTrace (runMachine mem [5])

main = do
    raw <- T.IO.readFile "input"
    let input = parse raw
        mem = listArray (0, (length input) - 1) input
    print $ part1 mem
    print $ part2 mem
