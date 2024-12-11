import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List as L
import Data.Char
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Text.Regex.Applicative (RE, string, sym, (<|>))
import Text.Regex.Applicative.Common (decimal)

main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile. head =<< getArgs

type Input = [String]

solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: String -> Input
parseEasy = lines

getResultEasy :: Input -> Int
getResultEasy = getResultWithCounterAndGridGen countXMAS (\g -> [g, diags g])

parseHard :: String -> Input
parseHard = parseEasy

getResultHard :: Input -> Int
getResultHard = getResultWithCounterAndGridGen countXMASCross (: [])

type Grid = [String]
type GridGen = Grid -> [Grid]

getResultWithCounterAndGridGen :: (Input -> Int) -> GridGen -> Input -> Int
getResultWithCounterAndGridGen counter gridGen = sum. map counter. grids gridGen

grids :: GridGen -> Grid -> [Grid]
grids gen = concatMap gen. take 4. iterate rotate90

countXMAS :: [String] -> Int
countXMAS = sum. map countXMASLine
    where countXMASLine = length. filter ("XMAS" `isPrefixOf`). tails

diags :: [[a]] -> [[a]]
diags [] = []
diags [a] = map (: []) a
diags ((x:xs):xss) = [x] : zipWith (:) xs beg ++ end
    where 
    (beg, end) = splitAt (length xs) (diags xss)

rotate90 :: [[a]] -> [[a]]
rotate90 = transpose. reverse

type TripleRow = (String, String, String)
type MiniGridCol = (Char, Char, Char)
type MiniGrid = (MiniGridCol, MiniGridCol, MiniGridCol)

countXMASCross :: [String] -> Int
countXMASCross = length. filter isX. prepMiniGrids

prepMiniGrids :: [String] -> [MiniGrid]
prepMiniGrids = concatMap tripleRowIntoMiniGrids. prepTripleRows
    where
    prepTripleRows :: [String] -> [TripleRow]
    prepTripleRows lst = zip3 lst (tail lst) (tail. tail $ lst)
    tripleRowIntoMiniGrids :: TripleRow -> [MiniGrid]
    tripleRowIntoMiniGrids = map (toCross. take 3). longTails. uncurry3 zip3
        where
        toCross [a, b, c] = (a, b, c)
        longTails = takeWhile ((>=3). length). tails
        uncurry3 f (a, b, c) = f a b c


isX :: MiniGrid -> Bool
isX (('M', _, 'M'),(_, 'A', _),('S',_,'S')) = True
isX _ = False
