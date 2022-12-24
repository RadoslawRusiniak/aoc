import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List.Split
import Data.List as L
import Data.Char (ord, isLower, isDigit)
import GHC.Utils.Misc (count)
import Data.Sequence as S
import Data.Foldable
import Data.List.Extra as LE
import Data.Maybe
import Data.Map as Mp

main = print. solve =<< getFileContents
    where getFileContents = readFile. head =<< getArgs


solve :: String -> Int
solve = getResult. parse

getResult :: [String] -> Int
getResult = getDirToDeleteSize. go
    where
    getDirToDeleteSize (mp, _) = minimum. LE.filter (>= (need mp)). elems $ mp
    need mp = 30000000 - (70000000 - mp ! "/")

type State = (Mp.Map String Int, [String])

go :: [String] -> State
go = LE.foldl handleCommand (Mp.empty, [])

handleCommand :: State -> String -> State
handleCommand acc c
    | (isDigit. head $ c) = handleFile acc (read c)
    | otherwise = handleCd acc c

handleFile :: State -> Int -> State
handleFile (mp, lst) v = (updatedMap, lst)
    where
    updatedMap = LE.foldl addValue mp lst
    addValue curmp key = Mp.insertWith (+) key v curmp 

handleCd :: State -> String -> State
handleCd (mp, (x:xs)) ".." = (mp, xs)
handleCd (mp, []) c = (mp, [c])
handleCd (mp, lst@(x:xs)) c = (mp, (x++"/"++c):lst)

parse :: String -> [String]
parse = LE.map toCommand. LE.filter isCdOrFile. LE.map words. lines

toCommand :: [String] -> String
toCommand ["$", "cd", d] = d
toCommand (f:_) = f

isCdOrFile :: [String] -> Bool
isCdOrFile s = isCd s || isFile s

isCd :: [String] -> Bool
isCd (_:s:_) = s == "cd"

isFile :: [String] -> Bool
isFile (f:_) = isDigit. head $ f
