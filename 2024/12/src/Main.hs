import Control.Monad
import Control.Monad.Loops (unfoldM)
import Control.Monad.State
import Data.Array
import Data.Char
import Data.Foldable
import Data.List as List
import Data.List.Extra (splitOn)
import Data.List.NonEmpty (unfold)
import Data.Array qualified as Array
import Data.Map qualified as Map
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Sequence (Seq)
import Data.Set qualified as Set
import Data.Set (Set)
import Linear.V2 (V2(..))
import System.Environment (getArgs)
import System.IO

main = do
  file <- getFileContents
  print . solveEasy $ file
  print . solveHard $ file
  where
    getFileContents = readFile . head =<< getArgs

type Point = V2 Int
type Parsed = Array Point Char

solveEasy = getResultEasy . parseEasy

solveHard = getResultHard . parseHard

parseEasy :: String -> Parsed
parseEasy input = Array.listArray bounds chars
    where
        chars = filter isAlpha input
        rows = lines input
        height = length rows
        width = length $ head rows
        bounds = (V2 1 1, V2 height width)

getResultEasy :: Parsed -> Int
getResultEasy = sum . map scoreEasy . groupRegions

parseHard :: String -> Parsed
parseHard = parseEasy

getResultHard :: Parsed -> Int
getResultHard = sum . map scoreHard . groupRegions

scoreEasy :: Set Point -> Int
scoreEasy region = Set.size region * componentPerimeter region
  where
    componentPerimeter region =
      length [ () | p <- Set.toList region, n <- neighbours p, n `Set.notMember` region ]

scoreHard :: Set Point -> Int
scoreHard region = Set.size region * componentSides
  where
    componentSides :: Int
    componentSides = sum $ map (length . connectedComponents . Set.fromList . neighboursInDirection) directions
    neighboursInDirection :: V2 Int -> [Point]
    neighboursInDirection d = filter (`Set.notMember` region) . map (+d) $ Set.toList region

groupRegions :: Array Point Char -> [Set Point]
groupRegions arr = concatMap connectedComponents groupedSets
  where
    pointGroups = [(c, Set.singleton p) | (p, c) <- Array.assocs arr]
    groupedSets = Map.elems $ Map.fromListWith Set.union pointGroups

connectedComponents :: Set Point -> [Set Point]
connectedComponents = unfoldr findConnectedComponent

findConnectedComponent :: Set Point -> Maybe (Set Point, Set Point)
findConnectedComponent searchSpace
  | Set.null searchSpace = Nothing
  | otherwise =
    let start = Set.findMin searchSpace
        component = bfs searchSpace start
        remaining = searchSpace Set.\\ component
        in Just (component, remaining)

type Queue = Seq Point
type Visited = Set Point
type BfsState = (Queue, Visited)
type BfsM a = State BfsState a
bfs :: Set Point -> Point -> Set Point
bfs searchSpace start = Set.fromList $ evalState (unfoldM step) initState
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
    getNeighbours all = filter (`Set.member` all) . neighbours

neighbours :: Point -> [Point]
neighbours p = [p + d | d <- directions]

directions :: [Point]
directions = [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]