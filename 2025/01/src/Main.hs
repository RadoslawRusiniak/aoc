import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.List as List
import Data.Char
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = [Rotation]

type Rotation = Int

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse = map parseLine . lines
  where
    parseLine ('L':val) = (-1) * read val
    parseLine ('R':val) = read val

getResultPart1 :: Parsed -> Int
getResultPart1 = check

getResultPart2 :: Parsed -> Int
getResultPart2 = const 0

check :: [Rotation] -> Int
check = length . filter isGood . scanl applyOp 50
  where
    isGood = (== 0)
    applyOp res rot = (res + rot) `mod` 100