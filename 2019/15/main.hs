-- This assumes a 64-bit Int
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Array
import Data.List
import Data.Maybe

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

data Point = Point Int Int deriving (Show,Eq)
data Tile = Tile Point Int deriving Show

seenTile :: [Tile] -> Point -> Bool
seenTile ts p@(Point x y) = any (\(Tile tp _) -> p == tp) ts

searchStep :: Machine -> Point -> [Tile] -> Int -> [Tile]
searchStep m (Point x y) ts dir = let
        p = case dir of
            1 -> Point x (y-1)
            2 -> Point x (y+1)
            3 -> Point (x-1) y
            4 -> Point (x+1) y
    in if seenTile ts p then ts
    else let
        (m', Output result) = step (pushInput m dir)
    --in case trace (show (result, dir, p)) result of
    in case result of
        0 -> (Tile p 1):ts
        1 -> search m' p ((Tile p 0):ts)
        2 -> (Tile p 2):ts

search :: Machine -> Point -> [Tile] -> [Tile]
search m p ts
    -- | trace (show p) False = undefined
    | otherwise = let
        x1 = searchStep m p ts 1
        x2 = searchStep m p x1 2
        x3 = searchStep m p x2 3
        x4 = searchStep m p x3 4
    in x4

nextTo :: Point -> Point -> Bool
nextTo (Point px py) (Point tx ty)
    | px /= tx && py /= ty = False
    | px == tx + 1 || px == tx - 1 = True
    | py == ty + 1 || py == ty - 1 = True
    | otherwise = False

nearAny :: [Point] -> Point -> Bool
nearAny ps t = any (nextTo t) ps

isOrigin :: [Point] -> Bool
isOrigin = any (== (Point 0 0))

walk :: ([Point] -> Bool) -> [Point] -> Int -> [Point] -> Int
walk cond path steps p = let
        (near, far) = partition (nearAny p) path
    in case cond near of
        True -> steps
        False -> walk cond far (steps+1) near

part1 path oxy = walk isOrigin path 1 [oxy]

isEmpty [] = True
isEmpty _ = False

part2 path oxy = walk isEmpty path 0 [oxy]

main = do
    raw <- T.IO.readFile "input"
    let input = parse raw
        -- 640 memory locations should be enough for anyone!
        extraMem = 640
        mem = listArray (0, (length input) + extraMem - 1) (input ++ (repeat 0))
        m = newMachine mem []
        start = Point 0 0
        ts = search m start [Tile start 0]
        path = [p | (Tile p n) <- ts, n == 0]
        Just (Tile oxy _) = find (\(Tile _ n) -> n == 2) ts
    print $ part1 path oxy
    print $ part2 path oxy
