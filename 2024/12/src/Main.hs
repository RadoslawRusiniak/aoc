import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.Array
import Data.List as List
import Data.List.Extra (splitOn)
import Data.Char
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Seq as Seq
import Text.Regex.Applicative (RE, string, sym, (<|>))
import Text.Regex.Applicative.Common (decimal)
import System.Console.Terminfo (Point)
import Text.XHtml (start)

main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile. head =<< getArgs

type Input = String

solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: String -> Input
parseEasy = id

getResultEasy :: Input -> Int
getResultEasy = const 0

parseHard :: String -> Input
parseHard = parseEasy

getResultHard :: Input -> Int
getResultHard = const 0

type Queue = Seq Point
type Visited = Set Point
type BfsState = (Queue, Visited)
bfs :: Set Point -> Point -> Set Point
bfs searchSpace start = Set.fromList $ unfoldr step (queue, visited)
    where
        queue = Seq.singleton start
        visited = Set.singleton start
        step :: BfsState -> Maybe (Point, BfsState)
        step queue visited =
            case Seq.viewl queue of
                Seq.EmptyL -> Nothing
                current Seq.:< rest ->
                    let neighbours = getNeighbours current
                        unvisited = filter (not. (`Set.member` visited)) neighbours
                        newQueue = rest Seq.>< Seq.fromList unvisited
                        newVisited = Set.union visited (Set.fromList unvisited)
                    in Just (current, (newQueue, newVisited))
