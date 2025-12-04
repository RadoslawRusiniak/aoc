import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List as L
import Data.Char
-- import GHC.Utils.Misc (count, nTimes)
import Data.Foldable
-- import Data.List.Extra as LE
import Data.Maybe
import Data.Array
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
-- import Algorithm.Search

main = print. solveEasy =<< getFileContents
    where getFileContents = readFile. head =<< getArgs

type Pos = (Int, Int)
add :: Pos -> Pos -> Pos
add (a, b) (x, y) = (a+x, b+y)
type Grid = Array Pos Int
type State = Grid
type Result = Int

solveEasy :: String -> Result
solveEasy = getResultEasy. parseEasy

solveHard :: String -> Result
solveHard = getResultHard. parseHard

getResultEasy :: State -> Result
getResultEasy = findShortestDistance

getResultHard :: State -> Result
getResultHard = getResultEasy

parseEasy :: String -> State
parseEasy = toArray. map (map digitToInt). lines
    where
    toArray lst = listArray ((1, 1), (length lst, length $ head lst)) (concat lst)

parseHard :: String -> State
parseHard = parseEasy

data Dir = U | R | D | L deriving (Enum, Show, Eq, Ord)
data Node = Node Pos Dir deriving (Eq, Ord)

data DijkstraState = DijkstraState 
    { visited :: Set.Set Node
    , distanceMap :: Map.Map Node Int
    , nodeQueue :: Set.Set (Int, Node)
    }

findShortestDistance :: Grid -> Int
findShortestDistance grid = minimum. map (resultMap Map.!) $ endNodes
    where
    start = (1, 1)
    end = snd. bounds $ grid
    endNodes = [Node end D, Node end R]
    startValueCost = grid ! start
    initialVisited = Set.empty
    initialDistance = Map.fromList [(Node start R, startValueCost), (Node start D, startValueCost)]
    initialQueue = Set.fromList [(startValueCost, Node start R), (startValueCost, Node start D)]
    initialState = DijkstraState initialVisited initialDistance initialQueue
    resultMap = processQueue initialState
    processQueue st@(DijkstraState v0 d0 q0) = case Set.minView q0 of
        Nothing -> d0
        Just ((dst, nd@(Node pos dir)), q1) -> if pos == end then d0
            else if Set.member nd v0 then processQueue (st { nodeQueue = q1 })
            else
                let v1 = Set.insert nd v0
                    neighbours = getNeighbours grid nd
                    unvisited = filter (\n -> not (Set.member n v0)) neighbours
                in processQueue $ foldl (foldNeighbour grid nd) (DijkstraState v1 d0 q1) unvisited

getNeighbours :: Grid -> Node -> [Node]
getNeighbours g (Node p d) = filter (isInbound g) $ allNeighbours
    where
        allNeighbours = [Node x y | y <- turns, x <- addDirectionTimes p y 3]
        turns = getTurnDirections d
        isInbound g (Node (x, y) _) = 1 <= x && x <= lenx && 1 <= y && y <= leny
        (_, (lenx, leny)) = bounds g

getTurnDirections dir
    | dir == R || dir == L = [U, D]
    | otherwise = [L, R]

addDirectionTimes :: Pos -> Dir -> Int -> [Pos]
addDirectionTimes p d t = take t. iterate (`addDirection` d) $ p

addDirection :: Pos -> Dir -> Pos
addDirection (px, py) d = (px + dx, py + dy)
    where (dx, dy) = directionToStep d

directionToStep U = (-1, 0)
directionToStep R = (0, 1)
directionToStep D = (1, 0)
directionToStep L = (0, -1)

inboundsNeighbours :: Grid -> [Node] -> [Node]
inboundsNeighbours g = filter (isInbound g)
    where