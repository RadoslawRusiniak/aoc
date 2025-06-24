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

main :: IO ()
main = do
    file <- getFileContents
    print . part1 $ file
    print . part2 $ file
    where getFileContents = readFile . head =<< getArgs

type Input = String
type Parsed = String

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse = id

getResultPart1 :: Parsed -> Int
getResultPart1 = const 0

getResultPart2 :: Parsed -> Int
getResultPart2 = const 0
