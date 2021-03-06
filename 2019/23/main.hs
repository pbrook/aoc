-- This assumes a 64-bit Int
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Array
import Data.List
import Data.Maybe
import Data.Char

import Debug.Trace

type Memory = Array Int Int

data MachineState = Running | Output [Int] | Stalled | Stopped deriving Show

data Machine = Machine {
    mPC :: Int,
    mBase :: Int,
    mMemory :: Memory,
    mInput :: [Int],
    mOutput :: [Int],
    mState :: MachineState,
    mTrace :: [MachineOp]}

data MachineOp = IncPC Int
    | Jump Int
    | MemWrite Int Int
    | PopInput
    | PushOutput Int
    | Stop
    | Stall
    | SetBase Int
    deriving Show

parse :: T.Text -> [Int]
parse = (map (read . T.unpack)) . (T.splitOn (T.singleton ','))

writeback :: Machine -> MachineOp -> Machine
writeback m@(Machine{mPC = pc}) (IncPC n) = m {mPC = pc + n}
writeback m@(Machine{mPC = pc}) (Jump x) = m {mPC = x}
writeback m@(Machine{mBase = base}) (SetBase x) = m {mBase = x}
writeback m@(Machine{mMemory = mem}) (MemWrite dest val) = m {mMemory = mem // [(dest, val)]}
writeback m@(Machine{mInput=inp}) PopInput = m {mInput = tail inp}
writeback m@(Machine{mOutput=out}) (PushOutput val) =
    if length out == 2
        then m {mState = (Output $ reverse (val:out)), mOutput=[]}
        else m {mOutput = val:out}
writeback m Stop = m {mState = Stopped}
writeback m Stall = m {mState = Stalled}

argType :: Int -> Int -> Int
argType op 1 = (op `div` 100) `mod` 10
argType op 2 = (op `div` 1000) `mod` 10
argType op 3 = (op `div` 10000) `mod` 10

getArg :: Machine -> Int -> Int
getArg (Machine {mMemory=mem, mPC=pc, mBase=base}) n = let
        op = mem ! pc
        imm = mem ! (pc + n)
    in case argType op n of
        0 -> mem ! imm
        1 -> imm
        2 -> mem ! (imm + base)

opWrite :: Machine -> Int -> Int -> MachineOp
opWrite (Machine {mPC=pc, mMemory=mem, mBase=base}) n val = let
        op = mem ! pc
        imm = mem ! (pc + n)
        addr = case argType op n of
            0 -> imm
            2 -> imm + base
    in MemWrite addr val

opBinary :: Machine -> (Int -> Int -> Int) -> [MachineOp]
opBinary m fn = let
        src1 = getArg m 1
        src2 = getArg m 2
        val = fn src1 src2
    in [opWrite m 3 val, (IncPC 4)]

opInput :: Machine -> [MachineOp]
opInput m@(Machine {mInput=input}) =
    case input of
        [] -> [Stall]
        x:xs -> [(opWrite m 1 x), PopInput, (IncPC 2)]

opSetBase :: Machine -> [MachineOp]
opSetBase m@(Machine {mBase=base}) = let
        inc = getArg m 1
    in [(SetBase (base + inc)), (IncPC 2)]

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

checkOp :: (Integer -> Integer -> Integer) -> Int -> Int -> Int
checkOp op a b = let
        a' = fromIntegral a :: Integer
        b' = fromIntegral b :: Integer
        c = a' `op` b'
        c' = fromIntegral c :: Int
    in case True of
        True
            | (fromIntegral c' :: Integer) == c -> c'

step :: Machine -> (Machine, MachineState)
step m@(Machine {mPC=pc, mMemory=mem}) = let
        op = mem ! pc
        mop = case op `mod` 100 of
            1 -> opBinary m (checkOp (+))
            2 -> opBinary m (checkOp (*))
            3 -> opInput m
            4 -> opOutput m
            5 -> opJump m (/=0)
            6 -> opJump m (==0)
            7 -> opBinary m (\x y -> if x < y then 1 else 0)
            8 -> opBinary m (\x y -> if x == y then 1 else 0)
            9 -> opSetBase m
            _ -> [Stop]
        m' = foldl' writeback (m {mState = Running}) mop
    in case mState m' of
        Running -> step m'
        s -> (m', s)

pushInput :: Machine -> Int -> Machine
--pushInput m@(Machine {mInput=i}) val = m {mInput = i ++ [val]}
pushInput m@(Machine {mInput=[]}) val = m {mInput = [val]}

pushArrayInput :: Machine -> [Int] -> Machine
pushArrayInput m@(Machine {mInput=i}) val = m {mInput = i ++ val}

setInput :: Machine -> [Int] -> Machine
setInput m val = m {mInput = val}

newMachine :: Memory -> [Int] -> Machine
newMachine mem inp = Machine {
    mMemory=mem,
    mPC=0,
    mBase=0,
    mInput=inp,
    mOutput=[],
    mTrace=[],
    mState=Running}

type Packet = [Int]

runMachine :: [Packet] -> Machine -> ([Packet], Machine)
runMachine p m = let
        (m', state) = step m
    in case state of
        Output val -> (val:p, m')
        Stalled -> (p, pushInput m' (-1))

insertPackets :: Int -> Machine -> [Packet] -> Machine
insertPackets addr m p = case filter (\x -> addr == head x) p of
        [] -> m
        p' -> pushArrayInput m (concat (map tail p'))

distPackets :: Int -> [Machine] -> [Packet] -> [Machine]
distPackets _ [] _ = []
distPackets _ m [] = m
distPackets addr (m:ms) p = let
        m' = insertPackets addr m p
    in m':(distPackets (addr+1) ms p)

execute :: [Machine] -> [Int]
execute c = let
        (p, c') = mapAccumL runMachine [] c
        result = find (\x -> head x == 255) p
    in case result of
        Nothing -> execute (distPackets 0 c' p)
        Just val -> val

part1 cluster = last $ execute cluster

insertNat (m:ms) nat = (pushArrayInput m nat):ms

execute2 :: Int -> [Int] -> [Machine] -> Int
execute2 lasty nat c = let
        (p, c') = mapAccumL runMachine [] c
    in case p of
        [] -> case nat of
            [] -> execute2 lasty nat c'
            [x,newy] -> case newy `compare` lasty of
                EQ -> newy
                _ -> execute2 newy nat (insertNat c' nat)
        _ -> let
                natp = find (\x -> head x == 255) p
                nn = maybe nat tail natp
            in execute2 lasty nn (distPackets 0 c' p)

part2 cluster = execute2 0 [] cluster

main = do
    raw <- T.IO.readFile "input"
    let input = parse raw
        extraMem = 100
        mem = listArray (0, (length input) + extraMem - 1) (input ++ (repeat 0))
        cluster = map (\n -> newMachine mem [n]) [0..49]
    print $ part1 cluster
    print $ part2 cluster
