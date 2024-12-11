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

main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile. head =<< getArgs

type Input = ()

solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: String -> Input
parseEasy = const ()

getResultEasy :: Input -> Int
getResultEasy = const 0

parseHard = parseEasy

getResultHard :: Input -> Int
getResultHard = const 0
