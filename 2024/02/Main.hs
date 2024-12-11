import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List as L
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Array
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

main = print. solveHard =<< getFileContents
    where getFileContents = readFile. head =<< getArgs

type ParsedInput = [[Int]]

solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: String -> ParsedInput
parseEasy = map (map read. words). lines

getResultEasy :: ParsedInput -> Int
getResultEasy = count isOk 

count :: (a -> Bool) -> [a] -> Int
count f = length. filter f

isOk :: [Int] -> Bool
isOk xs = isMonotonic xs && isClose xs

isMonotonic :: [Int] -> Bool
isMonotonic xs = isIncreasing || isDecreasing
	where
	isIncreasing = check (<=)
	isDecreasing = check (>=)
	check :: (Int -> Int -> Bool) -> Bool
	check f = all (uncurry f) pairs
	pairs :: [(Int, Int)]
	pairs = zip xs (tail xs)

isClose :: [Int] -> Bool
isClose = all (\d -> 1 <= d && d <= 3). diffs
	where
	diffs :: [Int] -> [Int]
	diffs xs = zipWith (\a b -> abs $ a - b) xs (tail xs)

parseHard = parseEasy

getResultHard :: ParsedInput -> Int
getResultHard = count anyIsOk

anyIsOk :: [Int] -> Bool
anyIsOk xs = isOk xs || any isOk (removedOne xs)

removedOne :: [Int] -> [[Int]]
removedOne [] = []
removedOne (x:xs) = xs : map (x:) (removedOne xs)
