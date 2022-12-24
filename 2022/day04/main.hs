import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List.Split
import Data.List
import Data.Char (ord, isLower)
import GHC.Utils.Misc (count)

main = do  
    contents <- getFileContents
    print. solve $ contents

getFileContents :: IO String
getFileContents = readFile . head =<< getArgs


solve :: String -> Int
solve = count overlap. parse

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap (p1, p2) = overlap' p1 p2 || overlap' p2 p1
    where overlap' (a, b) (c, d) = a <= c && c <= b

parse :: String -> [((Int, Int), (Int, Int))]
parse = map parseLine. lines
    where
    parseLine = toTuple. map readElf. splitOn ","
    readElf = toTuple. map (read :: String -> Int). splitOn "-"
    toTuple xs = (xs !! 0, xs !! 1)

