import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List.Split
import Data.List as L
import Data.Char
import GHC.Utils.Misc (count)
import qualified Data.Sequence as Sq
import Data.Foldable
import Data.List.Extra as LE
import Data.Maybe
import qualified Data.Map as Mp
import qualified Data.Set as St

main = print. solve =<< getFileContents
    where getFileContents = readFile. head =<< getArgs


solve :: String -> Int
solve = getResult. parse

type Matrix = Sq.Seq (Sq.Seq Int)
type State = (Matrix, Matrix)

getResult :: State -> Int
getResult st@(normal, transposed) = maximum. map (getScore st) $ indices
    where
    indices = [(x, y) | x <- [0 .. length normal - 1], y <- [0 .. length transposed - 1]]

getScore :: State -> (Int, Int) -> Int
getScore (normal, transposed) (x, y) = getScoreInRow normal (x, y) * getScoreInRow transposed (y, x)

getScoreInRow :: Matrix -> (Int, Int) -> Int
getScoreInRow m (x, y) = getScore left * getScore right
    where
    getScore xs
        | null xs = 0
        | all (< element) xs = length xs
        | otherwise = (1+). length. Sq.takeWhileL (< element) $ xs
    left = Sq.reverse. Sq.take y $ row
    right = Sq.drop (y+1) row
    element = Sq.index row y
    row = Sq.index m x

parse :: String -> State
parse = toState. getArr. lines
    where
    getArr :: [String] -> [[Int]]
    getArr = map (map digitToInt)

toState :: [[Int]] -> State
toState xs = (getSq xs, getSq. transpose $ xs)
    where
    getSq = Sq.fromList. map Sq.fromList
