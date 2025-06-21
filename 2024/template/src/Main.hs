import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.List as List
import Data.List.Extra (splitOn)
import Data.Char
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Linear.V2 (V2(..))
import Text.Regex.Applicative (RE, string, sym, (<|>))
import Text.Regex.Applicative.Common (decimal)

main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile. head =<< getArgs

type Input = String
type Parsed = String

solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: Input -> Parsed
parseEasy = id

getResultEasy :: Parsed -> Int
getResultEasy = const 0

parseHard :: Input -> Parsed
parseHard = parseEasy

getResultHard :: Parsed -> Int
getResultHard = const 0
