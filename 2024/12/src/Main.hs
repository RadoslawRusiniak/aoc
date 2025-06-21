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
import Data.List.NonEmpty (unfold)

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

groupRegions :: Array Point Char -> [Set Point]
groupRegions arr = concatMap bfsComponents groupedSets
  where
    pointGroups = [ (c, Set.singleton p) | (p, c) <- Array.assocs arr ]
    groupedSets = Map.elems $ Map.fromListWith Set.union pointGroups

bfsComponents :: Set Point -> [Set Point]
bfsComponents = unfoldr findConnectedComponent

findConnectedComponent :: Set Point -> (Set Point, Set Point)
findConnectedComponent searchSpace =
    let start     = Set.findMin searchSpace
        component = bfs searchSpace start
        remaining = searchSpace Set.\\ component
    in (component, remaining)

type Queue = Seq Point
type Visited = Set Point
type BfsState = (Queue, Visited)
type BfsM a = State BfsState a
bfs :: Set Point -> Point -> Set Point
bfs searchSpace start = Set.fromList $ evalState (unfoldrM step) initState
    where
        initState = (Seq.singleton start, Set.singleton start)
        step :: State BfsState (Maybe Point)
        step = do
            (queue, visited) <- get
            case Seq.viewl queue of
                Seq.EmptyL -> return Nothing
                current Seq.:< rest -> do
                    let neighbours = getNeighbours searchSpace current
                        unvisited = filter (`Set.notMember` visited) neighbours
                        newQueue = rest Seq.>< Seq.fromList unvisited
                        newVisited = foldr Set.insert visited unvisited
                    put (newQueue, newVisited)
                    return (Just current)
        getNeighbours = _