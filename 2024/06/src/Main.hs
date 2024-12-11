import System.IO  
import System.Environment (getArgs)
import Control.Monad
import qualified Data.Array as A
import Data.List as L
import Data.List.Extra (splitOn)
import Data.Char
import Data.Foldable
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Regex.Applicative (RE, string, sym, (<|>))
import Text.Regex.Applicative.Common (decimal)

main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile. head =<< getArgs

type Input = Lab
type Lab = A.Array Idx Char
type Idx = (Int, Int)

data Step = SUp | SRight | SDown | SLeft
toMv :: Step -> (Int, Int)
toMv SUp = (-1, 0)
toMv SRight = (0, 1)
toMv SDown = (1, 0)
toMv SLeft = (0, -1)

solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: String -> Input
parseEasy = listTo2DArray. lines

getResultEasy :: Input -> Int
getResultEasy = S.size. getAllPositions

getAllPositions :: Lab -> S.Set Idx
getAllPositions lab = S.fromList. unfoldr (uncurry (go lab)) $ (start, SUp)
    where
    start = fst. head. filter ((== '^'). snd). A.assocs $ lab

go :: Lab -> Idx -> Step -> Maybe (Idx, (Idx, Step))
go lab idx step =
    if (not. inBounds lab) idx then
        Nothing
    else
        Just (idx, getNextState lab idx step)
    where

getNextState :: Lab -> Idx -> Step -> (Idx, Step)
getNextState lab idx step = (nextPos, nextStep)
    where
    nextIdx = addStep idx step
    nextStep =
        if inBounds lab nextIdx && lab A.! nextIdx == '#' then
            rotate90 step 
        else
            step
    nextPos = addStep idx nextStep

inBounds :: Lab -> Idx -> Bool
inBounds lab (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2
    where ((x1, y1), (x2, y2)) = A.bounds lab

addStep :: Idx -> Step -> Idx
addStep (x, y) s = (x + sx, y + sy)
    where (sx, sy) = toMv s

rotate90 :: Step -> Step
rotate90 dir = case dir of
    SUp -> SRight
    SRight -> SDown
    SDown -> SLeft
    SLeft -> SUp

parseHard :: String -> Input
parseHard = parseEasy

getResultHard :: Input -> Int
getResultHard = const 0

listTo2DArray :: [[a]] -> A.Array (Int, Int) a
listTo2DArray xss =
    let
        rows = length xss
        cols = if null xss then 0 else length (head xss)
        bounds = ((1, 1), (rows, cols))
        elems = concat xss
    in
        A.listArray bounds elems
