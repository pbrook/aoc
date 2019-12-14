-- This assumes a 64-bit Int
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Array
import Data.List

type Memory = Array Int Int

data MachineState = Running | Output Int | Stopped

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

trace m@(Machine{mTrace=log}) op = writeback (m {mTrace=op:log}) op

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
    [(opWrite m 1 (head input)), PopInput, (IncPC 2)]

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
            9 -> opSetBase m
            _ -> [Stop]
        m' = foldl trace (m {mState = Running}) mop
    in case mState m' of
        Running -> step m'
        Output val -> (m', Just val)
        Stopped -> (m', Nothing)

setInput :: Machine -> Int -> Machine
setInput m val = m {mInput = repeat val}

newMachine :: Memory -> [Int] -> Machine
newMachine mem inp = Machine {
    mMemory=mem,
    mPC=0,
    mBase=0,
    mInput=inp,
    mTrace=[],
    mState=Running}

data Point = Point Int Int deriving Eq
data Pixel = Pixel Point Int
data BotPos = BotPos Point Int

findPixel :: Point -> [Pixel] -> Int
findPixel p c = let 
        c' = filter (\(Pixel p' d) -> p == p') c
    in case c' of
        [] -> 0
        (Pixel _ bw):_ -> bw

botStep :: Point -> Int -> BotPos
botStep (Point x y) d = let
        d' = d `mod` 4
        p = case d' of
            0 -> Point x (y - 1)
            1 -> Point (x + 1) y
            2 -> Point x (y + 1)
            3 -> Point (x - 1) y
    in BotPos p d'
        
runPainter :: Machine -> BotPos -> [Pixel] -> (Machine, [Pixel])
runPainter m (BotPos p d) px = let
        m' = setInput m (findPixel p px)
        (m1, o1) = step m'
        (m2, o2) = step m1
    in case o2 of
        Nothing -> (m2, px)
        Just val -> let
                Just color = o1
                Just turn = o2
                px' = (Pixel p color):px
                np = botStep p (d + (turn * 2) - 1)
            in runPainter m2 np px'

part1 mem = let
        (m, px) = runPainter (newMachine mem []) (BotPos (Point 0 0) 0) []
    in length (nub (map (\(Pixel p _) -> p) px))

main = do
    raw <- T.IO.readFile "input"
    let input = parse raw
        -- 640 memory locations should be enough for anyone!
        extraMem = 640
        mem = listArray (0, (length input) + extraMem - 1) (input ++ (repeat 0))
    print $ part1 mem
