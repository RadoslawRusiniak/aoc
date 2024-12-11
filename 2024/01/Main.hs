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

solveEasy = getResultEasy. parseEasy

parseEasy :: String -> [(Int, Int)]
parseEasy = map (f. map read. words). lines
	where f [a, b] = (a, b)

getResultEasy :: [(Int, Int)] -> Int
getResultEasy xs = sum. map abs $ zipWith (-) sortedFstArr sortedSndArr
	where
	firstArr = map fst xs
	sndArr = map snd xs
	sortedFstArr = sort firstArr
	sortedSndArr = sort sndArr 

solveHard = getResultHard. parseHard

parseHard :: String -> ([Int], [Int])
parseHard = getArrs. map (map read. words). lines
	where
	getArrs xs = (map head xs, map (head. tail) xs)

getResultHard :: ([Int], [Int]) -> Int
getResultHard (xs, ys) = sum $ map (getScore ys) xs
	where
	getScore arr x = x * (count x arr)
	count :: Int -> [Int] -> Int
	count _ [] = 0
	count x (y:rest) = (if x == y then 1 else 0) + (count x rest)
