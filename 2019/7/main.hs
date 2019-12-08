import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Array
import Data.List

type Memory = Array Int Int

data MachineState = Running | Output Int | Stopped

data Machine = Machine {
    mPC :: Int,
    mMemory :: Memory,
    mInput :: [Int],
    mState :: MachineState,
    mTrace :: [MachineOp]}

data MachineOp = IncPC Int
    | Jump Int
    | MemWrite Int Int
    | PopInput
    | PushOutput Int
    | Stop
    deriving Show

parse :: T.Text -> [Int]
parse = (map (read . T.unpack)) . (T.splitOn (T.singleton ','))

writeback :: Machine -> MachineOp -> Machine
writeback m@(Machine{mPC = pc}) (IncPC n) = m {mPC = pc + n}
writeback m@(Machine{mPC = pc}) (Jump x) = m {mPC = x}
writeback m@(Machine{mMemory = mem}) (MemWrite dest val) = m {mMemory = mem // [(dest, val)]}
writeback m@(Machine{mInput=inp}) PopInput = m {mInput = tail inp}
writeback m (PushOutput val) = m {mState = (Output val)}
writeback m Stop = m {mState = Stopped}

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

step :: Machine -> (Machine, Maybe Int)
step m@(Machine {mPC=pc, mMemory=mem}) = let
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
            _ -> [Stop]
        m' = foldl trace (m {mState = Running}) mop
    in case mState m' of
        Running -> step m'
        Output val -> (m', Just val)
        Stopped -> (m', Nothing)

pushInput :: Machine -> Int -> Machine
pushInput m@(Machine {mInput=i}) val = m {mInput = i ++ [val]}

newMachine :: Memory -> [Int] -> Machine
newMachine mem inp = Machine {mMemory=mem, mPC=0, mInput=inp, mTrace=[], mState=Running}

runMachine :: Memory -> [Int] -> Int
runMachine mem inp = let
        start = newMachine mem inp
        (end, Just out) = step start
    in out

runPhase :: Memory -> Int -> [Int] -> Int
runPhase mem val [] = val
runPhase mem val (phase:ps) = let
        next = runMachine mem [phase, val]
    in runPhase mem next ps

part1 mem = maximum (map (runPhase mem 0) (permutations [0..4]))

runAmp :: [Machine] -> Int -> Int
--runAmp :: [Machine] -> Int -> [(Int, [MachineOp])]
runAmp (a:ax) val = let
        (a', res) = step (pushInput a val)
    in case res of
        Nothing -> val
        Just newval -> runAmp (ax ++ [a']) newval
{-
        Nothing -> [(val, [])]
        Just newval -> (val, reverse $ mTrace a'):(runAmp (ax ++ [a']) newval)
-}

runPhase2 :: Memory -> [Int] -> Int
runPhase2 mem phase = let
        amps = map (\x -> newMachine mem [x]) phase
    in runAmp amps 0

--part2 mem = runPhase2 mem [5..9]
part2 mem = maximum (map (runPhase2 mem) (permutations [5..9]))

main = do
    raw <- T.IO.readFile "input"
    let input = parse raw
        mem = listArray (0, (length input) - 1) input
    print $ part1 mem
    print $ part2 mem
