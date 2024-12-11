import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.Array
import Data.List as L
import Data.List.Extra (splitOn)
import Data.Char
import Data.Foldable
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Text.Regex.Applicative (RE, string, sym, (<|>))
import Text.Regex.Applicative.Common (decimal)

main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile. head =<< getArgs

class AddSub a where
    add :: a -> a -> a
    sub :: a -> a -> a

instance (Num a, Num b) => AddSub (a, b) where
  add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  sub (x1, y1) (x2, y2) = add (x1, y1) (-x2, -y2)

type Row = String
type Grid = [Row]
type Input = Grid

solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: String -> Input
parseEasy = map (takeWhile (not. isSpace)). lines

getResultEasy :: Input -> Int
getResultEasy = length. getUniqueAntinodes generateAntinodesEasy

parseHard :: String -> Input
parseHard = parseEasy

getResultHard :: Input -> Int
getResultHard = length. getUniqueAntinodes generateAntinodesHard

type Idx = (Int, Int)
type AntennaType = Char
type AntennaPos = Idx
type AntennaPositions = [AntennaPos]
type Bounds = (Idx, Idx)

type AntinodesGenerator = Bounds -> AntinodesGeneratorWithBounds
type AntinodesGeneratorWithBounds = (AntennaPos, AntennaPos) -> [Idx]

getUniqueAntinodes :: AntinodesGenerator -> Grid -> S.Set Idx
getUniqueAntinodes gen xs = combine. map (getAntinodes (gen bnds)). getAntennasPos $ xs
    where
    combine = mconcat. map S.fromList
    bnds = getBounds xs

getAntinodes :: AntinodesGeneratorWithBounds -> AntennaPositions -> [Idx]
getAntinodes gen = concatMap gen. getPairs
    where getPairs xs = [(x, y) | x <- xs, y <- xs, x /= y]

generateAntinodesEasy :: AntinodesGenerator
generateAntinodesEasy bnds (a, b) = filter (withinBounds bnds) [b `add` (b `sub` a)]

generateAntinodesHard :: AntinodesGenerator
generateAntinodesHard bnds (a, b) = takeWhile (withinBounds bnds) $ iterate (`add` diff) b
    where diff = b `sub` a

withinBounds :: Bounds -> Idx -> Bool
withinBounds (topLeft, botRight) pos = topLeft `smaller` pos && pos `smaller` botRight
    where smaller (x1, y1) (x2, y2) = x1 <= x2 && y1 <= y2

getAntennasPos :: Grid -> [AntennaPositions]
getAntennasPos = M.elems. buildMap. nonDots. indexed2D
    where
    indexed2D xss = [((row, col), val) | (row, xs) <- zip [1..] xss, (col, val) <- zip [1..] xs]
    nonDots = filter ((/= '.'). snd)
    buildMap = M.fromListWith (<>). map (\(idx, c) -> (c, [idx]))

getBounds :: Grid -> Bounds
getBounds xss = ((1, 1), (rows, cols))
    where
    rows = length xss
    cols = if null xss then 0 else length (head xss)
