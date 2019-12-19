-- This assumes a 64-bit Int
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Array
import Data.List
import Data.Maybe
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

runMachine :: Machine -> [Int]
runMachine m = let
        (m', state) = step m
    in case state of
        Stopped -> []
        Output val -> val:(runMachine m')

data Point = Point Int Int deriving Show
type Grid = [[Char]]
type Step = String

cons3 a b c = [a, b, c]

findCross' :: Point -> Grid -> [Point]
findCross' _ [_, _] = []
findCross' p@(Point x y) (a:b:c:xs)
    | a == ".#." && b == "###" && c == ".#." = p:rest
    | otherwise = rest
    where rest = findCross' (Point (x+1) y) (b:c:xs)

findCross :: Int -> Grid -> [Point]
findCross _ [_, _] = []
findCross y (a:b:c:xs) = let
        p = findCross' (Point 1 y) (zipWith3 cons3 a b c)
    in p ++ (findCross (y+1) (b:c:xs))

alignment :: Point -> Int
alignment (Point x y) = x * y

part1 grid = let
        p = findCross 1 grid
    in sum (map alignment p)


-- Turning left rotates the grid right, and vice-versa
rotateGrid :: Char -> Grid -> Grid
rotateGrid 'L' = transpose.reverse
rotateGrid 'R' = reverse.transpose

rotatePos :: Char -> Grid -> (Int, Int) -> Int -> (Int, Int)
rotatePos 'L' g (x,y) n = let
        h = length g
        x' = h - (y + 1)
        y' = x - n
    in (x', y')
rotatePos 'R' g (x,y) n = let
        w = length (head g)
        x' = y
        y' = w - (x + 1 + n)
    in (x', y')


move :: Char -> Grid -> (Int, Int) -> Int -> [Step]
move turn grid p@(x,y) n = let
        s = turn:',':(show n)
        p' = rotatePos turn grid p n
        g' = rotateGrid turn grid
    in s:(advance g' p')

advance :: Grid -> (Int, Int) -> [Step]
advance grid p@(x,y) = let
        row = grid !! y
        l = reverse (take x row)
        r = drop (x+1) row
        ln = length (takeWhile (=='#') l)
        rn = length (takeWhile (== '#') r)
    in case ln `compare` rn of
        GT -> move 'L' grid p ln
        LT -> move 'R' grid p rn
        EQ -> []

fnLength :: [Step] -> Int
fnLength s = length (intercalate "," s)

gobble :: Int -> [Step] -> [Step]
gobble n s = let
        s' = take n s
    in if fnLength s' > 20 then [] else s'

munch :: [Step] -> [Step] -> (Int, [Step])
munch [] s = (0, s)
munch x s = case stripPrefix x s of
    Nothing -> (0, s)
    Just s' -> let
            (n, s'') = munch x s'
        in ((1+n), s'')

munchOne (n, s) x = let
        (n', s') = munch x s
    in ((n+n'), s')

munchMany :: [[Step]] -> [Step] -> [Step]
munchMany x s = let
        (n, s') = foldl munchOne (0, s) x
    in case n of
        0 -> s'
        _ -> munchMany x s'

munchAll :: ([Step], [Step], [Step]) -> [Step] -> Maybe String
munchAll _ [] = Just []
munchAll fn@(a, b, c) s = let
        (na, sa) = munch a s
        (nb, sb) = munch b sa
        (nc, sc) = munch c sb
    in if na + nb + nc == 0 then Nothing
    else case munchAll fn sc of
        Nothing -> Nothing
        Just rest -> Just ((replicate na 'A') ++ (replicate nb 'B') ++ (replicate nc 'C') ++ rest)

try :: [Step] -> (Int, Int, Int) -> Maybe (String, [Step], [Step], [Step])
try s (a, b, c) = let
        sa = gobble a s
        s' = munchMany [sa] s
        sb = gobble b s'
        s'' = munchMany [sa, sb] s'
        sc = gobble c s''
        r = munchAll (sa, sb, sc) s
    in case r of
        Nothing -> Nothing
        Just p
            | length p > 10 -> Nothing
            | otherwise -> Just (p, sa, sb, sc)

optimize s = let
        paths = [try s (a, b, c) | a <- [1..5] , b <- [1..5] , c <- [1..5]]
    in head (catMaybes paths)

walk :: Grid -> [Step]
walk grid = let
        bxs = map (findIndex (=='^')) grid
        (Just by) = findIndex isJust bxs
        bx = head (catMaybes bxs)
    in advance grid (bx, by)

runBot mem cmd = let
        mem' = mem // [(0, 2)]
        inp = map ord cmd
        m = newMachine mem' inp
        out = runMachine m
    in last out

makeCmd :: (String, [Step], [Step], [Step]) -> String
makeCmd (r, a, b, c) = let
        r' = intersperse ',' r
        fn = map (intercalate ",") [a, b, c]
    in (intercalate "\n" (r':fn)) ++ "\nn\n"

part2 mem grid = let
        steps = walk grid
        path = optimize steps
    in runBot mem (makeCmd path)

main = do
    --f <- readFile "test0"
    --print $ sum (map alignment (findCross 1 (lines f)))
    raw <- T.IO.readFile "input"
    let input = parse raw
        extraMem = 4096
        mem = listArray (0, (length input) + extraMem - 1) (input ++ (repeat 0))
        m = newMachine mem []
        out = lines (map chr (runMachine m))
        grid = filter (/= "") out
    print $ part1 grid
    print $ part2 mem grid
