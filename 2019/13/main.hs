-- This assumes a 64-bit Int
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Array
import Data.List

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

step :: Machine -> (Machine, MachineState)
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
        m' = foldl' trace (m {mState = Running}) mop
    in case mState m' of
        Running -> step m'
        s -> (m', s)

pushInput :: Machine -> Int -> Machine
pushInput m@(Machine {mInput=i}) val = m {mInput = i ++ [val]}

newMachine :: Memory -> [Int] -> Machine
newMachine mem inp = Machine {
    mMemory=mem,
    mPC=0,
    mBase=0,
    mInput=inp,
    mTrace=[],
    mState=Running}

runMachine :: Machine -> (Machine, MachineState, [Int])
runMachine m = let
        (m', state) = step m
    in case state of
        Output val -> let
                (m'', s', rest) = runMachine m'
            in (m'', s', val:rest)
        _ -> (m', state, [])

data Tile = Tile Int Int Int deriving Show

makeTiles :: [Int] -> [Tile]
makeTiles [] = []
makeTiles (a:b:c:s) = (Tile a b c):(makeTiles s)

lastTiles :: [Tile] -> [Tile]
lastTiles [] = []
lastTiles (t:rest) = let
        Tile x y c = t
        rf = filter (\(Tile x' y' _) -> (x /= x') || (y /= y')) rest
    in t:(lastTiles rf)

part1 mem = let
        m = newMachine mem []
        (_, _, out) = runMachine m
        t = makeTiles out
        last = lastTiles t
        blocks = filter (\(Tile _ _ c) -> c == 2) last
    in length blocks

tileChar :: Tile -> Char
tileChar (Tile _ _ c) =
    case c of
        0 -> '.'
        1 -> '#'
        2 -> '%'
        3 -> '-'
        4 -> 'O'

tileSort (Tile x1 y1 _) (Tile x2 y2 _) =
    case y1 `compare` y2 of
        EQ -> x1 `compare` x2
        c -> c

render :: [Tile] -> IO ()
render [] = return ()
render t = do
    let st = sortBy tileSort t
        (x, xs) = splitAt 44 st
    putStrLn $ map tileChar x
    render xs

nextInput [] _ = (0, [])
nextInput x@((a, b):xs) c
    | a == c = (b, xs)
    | otherwise = (0, x)

data GameResult = Lost Int Int | Won Int

playPlan m _ [] = m
playPlan m step plan = let
        (inp, plan') = nextInput plan step
        (m', _, _) = runMachine (pushInput m inp)
    in playPlan m' (step + 1) plan'
    

findScore :: [Int] -> Int
findScore out = let
        t = makeTiles out
        ((Tile _ _ score):[]) = filter (\(Tile x _ _) -> x == -1) t
    in score

tryGame m step = let
        (m', ms, out) = runMachine (pushInput m 0)
    in case ms of
        Stopped -> Won (findScore out)
        _ -> case out of
            [x, 17, 0, _, 18, 4] -> Lost x step
            _ -> tryGame m' (step + 1)

makePlan :: Int -> Int -> Int -> [(Int, Int)]
makePlan pos newpos when = let
        dist = abs (newpos - pos)
        dir = signum (newpos - pos)
    in [(c, dir) | c <- [(when + 1 - dist)..when]]

winGame :: Machine -> Int -> Int
winGame m paddle = do
    case tryGame m 0 of
        Won score -> score
        Lost newpos when -> let
                plan = makePlan paddle newpos when
                m' = playPlan m 0 plan
            in winGame m' newpos

playGame :: Memory -> IO ()
playGame mem = do
    let m = newMachine mem []
        (m', _, out) = runMachine m
        t = makeTiles out
        ((Tile paddle _ _):[]) = filter (\(Tile _ _ c) -> c == 3) t
    --render $ tail t
    print (winGame m' paddle)


main = do
    raw <- T.IO.readFile "input"
    let input = parse raw
        -- 640 memory locations should be enough for anyone!
        extraMem = 640
        mem = listArray (0, (length input) + extraMem - 1) (input ++ (repeat 0))
    print $ part1 mem
    playGame (mem // [(0,2)])
