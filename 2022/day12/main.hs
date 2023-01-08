import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List as L
import Data.Char
import GHC.Utils.Misc (count, nTimes)
import Data.Foldable
import Data.List.Extra as LE
import Data.Maybe
import Data.Array
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Algorithm.Search

main = print. solveHard =<< getFileContents
    where getFileContents = readFile. head =<< getArgs

type Index = (Int, Int)
type Start = Index
type End = Index
type Grid = Array Index Char
type State = (Grid, Start, End)
type CanMove = Char -> Char -> Bool
type Result = Maybe Int

solveEasy :: String -> Result
solveEasy = getResultEasy. parseEasy

solveHard :: String -> Result
solveHard = getResultHard. parseHard

getResultEasy :: State -> Result
getResultEasy (grid, start, end) = length <$> bfs (neighbours grid canClimb) (== end) start

getResultHard :: State -> Result
getResultHard (grid, _, end) = length <$> bfs (neighbours grid canDescend) isLowest end
    where
    isLowest idx = grid ! idx == 'a'

neighbours :: Grid -> CanMove -> Index -> [Index]
neighbours grid canMove cur@(x, y) = filter canGo [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    where
    canGo nxt = inBounds nxt && canMove (grid ! cur) (grid ! nxt)
    inBounds = inRange (bounds grid)

canClimb :: Char -> Char -> Bool
canClimb cur nxt = ord cur + 1 >= ord nxt

canDescend :: Char -> Char -> Bool
canDescend cur nxt = canClimb nxt cur

parseEasy :: String -> State
parseEasy input = (simplifiedGrid, startIndex, endIndex)
    where
    grid = listArray ((1, 1), (rows, columns)) elems
        where
        asLines = lines input
        rows = length asLines
        columns = length. head $ asLines
        elems = concat asLines
    startIndex = getIndex (== 'S') grid
    endIndex = getIndex (== 'E') grid
    getIndex match = fst. fromJust. LE.find (match. snd). assocs
    simplifiedGrid = grid // [(startIndex, 'a'), (endIndex, 'z')]

parseHard :: String -> State
parseHard = parseEasy

