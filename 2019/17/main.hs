-- This assumes a 64-bit Int
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Array
import Data.List
import Data.Char

import Debug.Trace

type Memory = Array Int Int

data MachineState = Running | Output Int | Stalled | Stopped deriving Show

data Machine = Machine {
    mPC :: Int,
    mBase :: Int,
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
writeback m (PushOutput val) = m {mState = (Output val)}
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

newMachine :: Memory -> [Int] -> Machine
newMachine mem inp = Machine {
    mMemory=mem,
    mPC=0,
    mBase=0,
    mInput=inp,
    mTrace=[],
    mState=Running}

runMachine :: Machine -> String
runMachine m = let
        (m', state) = step m
    in case state of
        Stopped -> []
        Output val -> (chr val):(runMachine m')

pad' :: a -> [a] -> [a]
pad' a b = (a:b) ++ [a]

pad :: [String] -> [String]
pad grid = let
        len = 2 + length (head grid)
        blank = replicate len '.'
    in pad' blank (map (pad' '.') grid)

data Point = Point Int Int deriving Show

cons3 a b c = [a, b, c]

findCross' :: Point -> [String] -> [Point]
findCross' _ [_, _] = []
findCross' p@(Point x y) (a:b:c:xs)
    | a == ".#." && b == "###" && c == ".#." = p:rest
    | otherwise = rest
    where rest = findCross' (Point (x+1) y) (b:c:xs)
        

findCross :: Int -> [String] -> [Point]
findCross _ [_, _] = []
findCross y (a:b:c:xs) = let
        p = findCross' (Point 1 y) (zipWith3 cons3 a b c)
    in p ++ (findCross (y+1) (b:c:xs))

alignment :: Point -> Int
alignment (Point x y) = x * y

part1 mem = let
        m = newMachine mem []
        out = lines (runMachine m)
        grid = id (filter (/= "") out)
        p = findCross 1 grid
    in sum (map alignment p)

main = do
    --f <- readFile "test0"
    --print $ sum (map alignment (findCross 1 (lines f)))
    raw <- T.IO.readFile "input"
    let input = parse raw
        extraMem = 4096
        mem = listArray (0, (length input) + extraMem - 1) (input ++ (repeat 0))
    print $ part1 mem
