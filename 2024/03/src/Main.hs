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
import Text.Regex.Applicative (RE, string, sym, findFirstInfix, (<|>))
import Text.Regex.Applicative.Common (decimal)

main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile. head =<< getArgs

type Input = String

data Op =
    Do
    | Dont
    | Mul Int Int
    deriving (Eq, Show)

solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: String -> Input
parseEasy = id

getResultEasy :: Input -> Int
getResultEasy = sum. map runOp. getAll parseOp

parseHard = parseEasy

parseOp :: RE Char Op
parseOp = parseDo <|> parseDont <|> parseMul
    where
    parseDo = Do <$ string "do()" 
    parseDont = Dont <$ string "don't()"
    parseMul = Mul <$> (string "mul(" *> decimal) <*> (string "," *> decimal <* string ")")

runOp :: Op -> Int
runOp (Mul x y) = x * y
runOp _ = error "wrongOp"

getAll :: RE Char a -> String -> [a]
getAll re = unfoldr getNextMatch
    where
    getNextMatch = fmap getUnfoldPair. findFirstInfix re
    getUnfoldPair (_, m, rest) = (m, rest)

getResultHard :: Input -> Int
getResultHard = sum. map runOp. runOpFilter. getAll parseOp

runOpFilter :: [Op] -> [Op]
runOpFilter = runFilter True
    where
    runFilter _ [] = []
    runFilter _ (Do : xs) = runFilter True xs
    runFilter _ (Dont : xs) = runFilter False xs
    runFilter False (_ : xs) = runFilter False xs
    runFilter True (mul : xs) = mul : runFilter True xs
