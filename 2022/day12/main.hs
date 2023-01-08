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

main = print. solveEasy =<< getFileContents
    where getFileContents = readFile. head =<< getArgs

type Index = (Int, Int)
type Start = Index
type End = Index
type Grid = Array Index Char
type StateEasy = (Grid, Start, End)
type IsEnd = Index -> Bool
type StateHard = (Grid, Start)
type CanMove = Char -> Char -> Bool

type Result = Maybe Int
solveEasy :: String -> Result
solveEasy = getResultEasy. parseEasy

solveHard :: String -> Result
solveHard = getResultHard. parseHard

getResultEasy :: StateEasy -> Result
getResultEasy (grid, start, end) = fmap length $ bfs (neighboursEasy grid) (== end) start

getResultHard :: StateHard -> Result
getResultHard (grid, start) = fmap length $ bfs (neighboursHard grid) isEnd start
    where
    isEnd idx = element == 'a' || element == 'S'
        where element = grid ! idx

neighboursEasy :: Grid -> Index -> [Index]
neighboursEasy grid cur = neighbours grid cur canClimb

neighboursHard :: Grid -> Index -> [Index]
neighboursHard grid cur = neighbours grid cur canDescend

neighbours :: Grid -> Index -> CanMove -> [Index]
neighbours grid cur@(x, y) canMove =
    filter canGo [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    where
    canGo idx = inBounds idx && canMove (grid ! cur) (grid ! idx)
    inBounds = inRange (bounds grid)

canClimb :: Char -> Char -> Bool
canClimb cur 'E' = cur == 'z'
canClimb 'S' nxt = nxt == 'a'
canClimb cur nxt = ord cur + 1 >= ord nxt

canDescend :: Char -> Char -> Bool
canDescend cur nxt = canClimb nxt cur

parseEasy :: String -> StateEasy
parseEasy input = (grid, startIndex, endIndex)
    where
    grid = getGrid input
    startIndex = getIndex (== 'S'). assocs $ grid
    endIndex = getIndex (== 'E'). assocs $ grid

parseHard :: String -> StateHard
parseHard input = (grid, startIndex)
    where
    grid = getGrid input
    startIndex = getIndex (== 'E'). assocs $ grid

getGrid :: String -> Grid
getGrid input = grid
    where
    asLines = lines input
    rows = length asLines
    columns = length. head $ asLines
    elems = concat asLines
    grid = listArray ((1, 1), (rows, columns)) elems

getIndex :: (a -> Bool) -> [(Index, a)] -> Index
getIndex f = fromJust. fmap fst. LE.find (f. snd)
