-- This assumes a 64-bit Int
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Array
import Data.List
import Data.Maybe
import Data.Char

import Debug.Trace

type Memory = Array Int Int

data MachineState = Running
--    | Output Int
    | Stalled
    | Stopped
    deriving Show

data Machine = Machine {
    mPC :: Int,
    mBase :: Int,
    mMemory :: Memory,
    mInput :: [Int],
    mOutput :: [Int],
    mState :: MachineState,
    mTrace :: [MachineOp]} deriving Show

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
writeback m@(Machine{mMemory = mem}) (MemWrite dest val) = seq dest seq val m {mMemory = mem // [(dest, val)]}
writeback m@(Machine{mInput=inp}) PopInput = m {mInput = tail inp}
writeback m@(Machine{mOutput=out}) (PushOutput val) = seq val m {mOutput = val:out}
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

setInput :: Machine -> [Int] -> Machine
setInput m val = m {mInput = val}

clearOutput m = m {mOutput=[]}

newMachine :: Memory -> [Int] -> Machine
newMachine mem inp = Machine {
    mMemory=mem,
    mPC=0,
    mBase=0,
    mInput=inp,
    mOutput=[],
    mTrace=[],
    mState=Running}

play m = do
    let
        (m', state) = step m
        out = reverse $ mOutput m'
    putStr $ map chr out
    case state of
        Stopped -> return ()
        Stalled -> do
            c <- getChar
            play $ clearOutput $ pushInput m' (ord c)

pathCmd 'e' = "east"
pathCmd 'w' = "west"
pathCmd 'n' = "north"
pathCmd 's' = "south"
pathRCmd 'e' = "west"
pathRCmd 'w' = "east"
pathRCmd 'n' = "south"
pathRCmd 's' = "north"

getItem (name, path) = (map pathCmd path) ++ ["take "++name] ++ (map pathRCmd (reverse path))

data Action = Start | Door String | Item String | Checkpoint String deriving (Show,Eq)

examine :: String -> Maybe Action
examine s = case stripPrefix "- " s of
    Nothing -> if s ==  "== Security Checkpoint =="
        then Just (Checkpoint "")
        else Nothing
    Just name
        | name `elem` ["north","south","east","west"] -> Just $ Door name
        | name == "infinite loop" -> Nothing
        | name == "giant electromagnet" -> Nothing
        | otherwise -> Just $ Item name

reverseAction Start = Start
reverseAction (Door "north") = Door "south"
reverseAction (Door "south") = Door "north"
reverseAction (Door "east") = Door "west"
reverseAction (Door "west") = Door "east"

actionString :: Action -> String
actionString (Door name) = name
actionString (Item name) = "take " ++ name
actionString Start = "inv"
actionString (Checkpoint name) = name

cmdString :: [Action] -> [Int]
cmdString = map ord . unlines . map actionString

getOutput :: Machine -> String
getOutput = reverse . map chr . mOutput

tryAction :: [Action] -> Machine -> Action -> [[Action]]
tryAction path m a = let
        (m', status) = step (clearOutput $ setInput m (cmdString [a]))
        path' = a:path
    in case a of
        Item _ -> case status of
            Stopped -> []
            Stalled -> [path']
        Door _ -> explore path' m'

isCheckpoint :: Action -> Bool
isCheckpoint (Checkpoint _) = True
isCheckpoint _ = False

exploreCheckpoint :: [Action] -> [Action] -> [[Action]]
exploreCheckpoint path a = let
        (Just (Door name)) = find (not.isCheckpoint) a
    in [(Checkpoint name):path]

explore :: [Action] -> Machine -> [[Action]]
explore path m = let
        out = getOutput m
        actions = mapMaybe examine (lines out)
        a = filter (/= reverseAction (head path)) actions
    in if any isCheckpoint a
        then exploreCheckpoint path a
        else concatMap (tryAction path m) a

fetchItem a = (reverse a) ++ (map reverseAction (tail a))

collectItems m cp [] = let
        a = reverse (tail cp)
        (m', Stalled) = step (clearOutput $ setInput m $ cmdString a)
    in m'
collectItems m cp (i:xs) = let
        ic = fetchItem i
        (m', Stalled) = step (clearOutput $ setInput m $ cmdString ic)
    in collectItems m' cp xs

itemName :: Action -> Maybe String
itemName (Item name) = Just name
itemName _ = Nothing

tryCheckpoint :: Machine -> String -> [[String]] -> String
tryCheckpoint m name (i:ix) = let
        cmd = map ord (unlines (i ++ [name]))
        (m', status) = step (setInput m cmd)
    in case status of
        Stopped -> filter (isDigit) (getOutput m')
        Stalled -> tryCheckpoint m name ix

forceCheckpoint m (Checkpoint name) i = let
        m' = clearOutput m
        ic = map ("drop " ++) i
    in tryCheckpoint m' name (subsequences ic)

main = do
    raw <- T.IO.readFile "input"
    let input = parse raw
        extraMem = 500
        mem = listArray (0, (length input) + extraMem - 1) (input ++ (repeat 0))
        m = newMachine mem []
        (m', state) = step m
        poi = explore [Start] m'
        ([cp], items) = partition (isCheckpoint.head) poi
        cpm = collectItems m' cp items
        i = mapMaybe (itemName.head) items
        password = forceCheckpoint cpm (head cp) i
    --putStr $ getOutput cpm
    --play cpm
    putStrLn password
