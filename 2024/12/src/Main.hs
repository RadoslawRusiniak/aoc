import Control.Monad.Loops (unfoldM)
import Control.Monad.State
import Data.Array
import Data.Char
import Data.List qualified as List
import Data.Array qualified as Array
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Sequence (Seq)
import Data.Set qualified as Set
import Data.Set (Set)
import Linear.V2 (V2(..))
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- getFileContents
  print . solveEasy $ file
  print . solveHard $ file
  where
    getFileContents = readFile . head =<< getArgs

type Input = String
type Point = V2 Int
type Dir = V2 Int
type Parsed = Array Point Char

solveEasy, solveHard :: Input -> Int
solveEasy = getResultEasy . parseEasy
solveHard = getResultHard . parseHard

parseEasy :: String -> Parsed
parseEasy input = Array.listArray arrBounds chars
    where
        chars = filter isAlpha input
        rows = lines input
        height = length rows
        width = length $ head rows
        arrBounds = (V2 1 1, V2 height width)

getResultEasy :: Parsed -> Int
getResultEasy = sum . map scoreEasy . groupRegions

parseHard :: String -> Parsed
parseHard = parseEasy

getResultHard :: Parsed -> Int
getResultHard = sum . map scoreHard . groupRegions

scoreEasy :: Set Point -> Int
scoreEasy = getScore (\region dir -> Set.size $ neighboursInDirection region dir)

scoreHard :: Set Point -> Int
scoreHard = getScore sidesInDirection
  where
    sidesInDirection region = length . connectedComponents . neighboursInDirection region

type ScoreF = Set Point -> Dir -> Int

getScore :: ScoreF -> Set Point -> Int
getScore f region = Set.size region * sum [ f region dir | dir <- directions ]

neighboursInDirection :: Set Point -> Dir -> Set Point
neighboursInDirection r d = Set.map (+d) r Set.\\ r

groupRegions :: Array Point Char -> [Set Point]
groupRegions arr = concatMap connectedComponents groupedSets
  where
    pointGroups = [(c, Set.singleton p) | (p, c) <- Array.assocs arr]
    groupedSets = Map.elems $ Map.fromListWith Set.union pointGroups

connectedComponents :: Set Point -> [Set Point]
connectedComponents = List.unfoldr findConnectedComponent

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
          let neighbours = getNeighbours current
              unvisited = filter (`Set.notMember` visited) neighbours
              newQueue = rest Seq.>< Seq.fromList unvisited
              newVisited = foldr Set.insert visited unvisited
          put (newQueue, newVisited)
          return (Just current)
    getNeighbours = filter (`Set.member` searchSpace) . adjacent

adjacent :: Point -> [Point]
adjacent p = [p + d | d <- directions]

directions :: [Dir]
directions = [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]