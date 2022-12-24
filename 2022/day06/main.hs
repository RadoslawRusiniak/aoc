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


solve :: String -> Int
solve s = getResult 14 (LE.take 13 s) (LE.drop 13 s)

getResult :: Int -> [Char] -> String -> Int
getResult cnt prev (x:xs) = 
    if nub (x:prev) == (x:prev)
    then cnt
    else getResult (cnt+1) (x:(LE.dropEnd 1 prev)) xs
