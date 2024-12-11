import System.IO  
import System.Environment (getArgs)
import Control.Applicative (many)
import Control.Monad
import Data.List as L
import Data.List.Extra (splitOn)
import Data.Char
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Text.Regex.Applicative (RE, match, string, sym, (<|>))
import Text.Regex.Applicative.Common (decimal)

main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile. head =<< getArgs

type Input = [Equation]
type Equation = (Result, [Element])
type Result = Int
type Element = Int

solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: String -> Input
parseEasy = maybe (error "parsing") id. match parser

parser :: RE Char Input 
parser = many (equation <* string "\n")
    where
    equation = (,) <$> (result <* string ": ") <*> elements
    result = decimal
    elements = (:) <$> decimal <*> many (sym ' ' *> decimal)

getResultEasy :: Input -> Int
getResultEasy = sum. map toScore. filter (isOk [(*), (+)])

type Operation = Int -> Int -> Int
type AllowedOps = [Operation]

isOk :: AllowedOps -> Equation -> Bool
isOk ops (r, x:xs) = go xs x
    where
    go [] c = r == c
    go (x:xs) c = any (go xs. (\op -> op c x)) ops

toScore :: Equation -> Int
toScore = fst

parseHard :: String -> Input
parseHard = parseEasy

getResultHard :: Input -> Int
getResultHard = sum. map toScore. filter (isOk [(*), (+), concatOp])

concatOp x y = x * 10^(length. show $ y) + y
