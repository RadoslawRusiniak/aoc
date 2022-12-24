import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List.Split
import Data.List as L
import Data.Char
import GHC.Utils.Misc (count, nTimes)
import qualified Data.Sequence as Sq
import Data.Foldable
import Data.List.Extra as LE
import Data.Maybe
import qualified Data.Map as Mp
import qualified Data.Set as St

main = print. solve =<< getFileContents
    where getFileContents = readFile. head =<< getArgs


type Pos = (Int, Int)
type TailPositions = St.Set Pos
type State = (Sq.Seq Pos, TailPositions)

type Direction = Char
type StepsCount = Int
type Command = (Direction, StepsCount)

solve :: String -> Int
solve = getResult initState. parse
    where 
    initState = (initAllPos, St.fromList [initPos])
    initAllPos = Sq.fromList $ replicate 10 initPos
    initPos = (0, 0)

parse :: String -> [Command]
parse = map toCommand. lines
    where toCommand = (\[dir, cnt] -> (head dir, read cnt)). words

getResult :: State -> [Command] -> Int
getResult st = getUniquePositions. foldl handleCommand st
    where getUniquePositions (_, st) = length st

handleCommand :: State -> Command -> State
handleCommand s (dir, cnt) = nTimes cnt (move dir) s

move :: Direction -> State -> State
move d (allpos, lasttposs) = (movedWithTails, addedTailPos)
    where
    movedWithTails = updateTails movedHead
        where
        hd = Sq.index allpos 0 
        headShifted = shiftHead hd d
        movedHead = Sq.update 0 headShifted allpos
    addedTailPos = St.insert (Sq.index movedWithTails 9) lasttposs
     
updateTails :: Sq.Seq Pos -> Sq.Seq Pos
updateTails allpos = foldl updateTail allpos [1 .. 9]

updateTail :: Sq.Seq Pos -> Int -> Sq.Seq Pos
updateTail allpos idx = Sq.update idx (moveTail prev cur) allpos
    where
    prev = Sq.index allpos (idx-1)
    cur = Sq.index allpos idx    

shiftHead :: Pos -> Direction -> Pos
shiftHead hpos d = addTuples hpos shift 
    where
    shift = directions Mp.! d
    directions = Mp.fromList [
        ('U', (1, 0)), 
        ('R', (0, 1)), 
        ('D', (-1, 0)), 
        ('L', (0, -1))]
    addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

moveTail :: Pos -> Pos -> Pos
moveTail (hx, hy) tpos@(tx, ty) = if needTailShift then shiftTail else tpos
    where
    needTailShift = abs(hx - tx) > 1 || abs(hy - ty) > 1
    shiftTail = (shiftTailDimension tx hx, shiftTailDimension ty hy)
        where shiftTailDimension tv hv = tv + fromEnum(hv > tv) - fromEnum(tv > hv)
