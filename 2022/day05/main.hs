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

main = print. solve =<< getFileContents
    where getFileContents = readFile. head =<< getArgs


solve :: String -> String
solve = getTop. processCommands. getStacksAndCommands

getTop :: Seq [Char] -> String
getTop = map head. toList

processCommands :: ([(Int, Int, Int)], Seq [Char]) -> Seq [Char]
processCommands (commands, stacks) = foldl process stacks commands

getStacksAndCommands :: String -> ([(Int, Int, Int)], Seq [Char])
getStacksAndCommands = (\(s:c:[]) -> (getCommands c, getStacks s)). LE.splitOn [emptyLine]. lines
    where emptyLine = ""

getStacks :: [String] -> Seq [Char]
getStacks = readSeq. toPerStackList. map parseLine. LE.dropEnd 1
    where
    readSeq = S.fromList. map (map fromJust. dropWhile (== Nothing)) 
    toPerStackList = transpose
    parseLine = map (parseBox. trim). LE.chunksOf 4
    parseBox b = if (b == "") then Nothing else (return $ getCharFromBox b)
    getCharFromBox :: String -> Char
    getCharFromBox (_:c:_) = c

getCommands :: [String] -> [(Int, Int, Int)]
getCommands = map getCommand
getCommand :: String -> (Int, Int, Int)
getCommand = (\[a,b,c] -> (a, b, c)). map read. L.filter (isDigit. head). words

process :: Seq [Char] -> (Int, Int, Int) -> Seq [Char]
process xs (cnt, s, d) = insertElements. removeElements $ xs
    where
    (toMove, withRemoved) = LE.splitAt cnt toRemove
    toRemove = index xs (s-1)
    toAdd = index xs (d-1)
    withAdded = toMove ++ toAdd
    removeElements = update (s-1) withRemoved
    insertElements = update (d-1) withAdded
