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

type Result = Maybe Int
solveEasy :: String -> Result
solveEasy = getResultEasy. parseEasy

getResultEasy :: StateEasy -> Result
getResultEasy (grid, start, end) = fmap length $ bfs (neighbours grid) (== end) start

neighbours :: Grid -> Index -> [Index]
neighbours grid cur@(x, y) = 
    filter canGo $ [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    where
    canGo idx = inBounds idx && notSteep (grid ! cur) (grid ! idx)
    inBounds = inRange (bounds grid)
    notSteep curV 'E' = curV == 'z'
    notSteep 'S' nxtV = nxtV == 'a'
    notSteep curV nxtV = ord curV + 1 >= ord nxtV

parseEasy :: String -> StateEasy
parseEasy input = (grid, startIndex, endIndex)
    where
    asLines = lines input
    rows = length asLines
    columns = length. head $ asLines
    elems = concat asLines
    grid = listArray ((1, 1), (rows, columns)) elems
    startIndex = getIndex (== 'S'). assocs $ grid
    endIndex = getIndex (== 'E'). assocs $ grid
    getIndex f = fromJust. fmap fst. LE.find (f. snd)
