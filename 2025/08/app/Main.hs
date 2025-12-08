import System.Environment (getArgs)
import Data.Array as Array
import Data.List (group, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (Down(..), comparing)
import Linear.Metric (qd)
import Linear.V3 (V3(..))

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = Boxes

type Boxes = [Box]
type Box = V3 Int

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse = map parseBox . lines

parseBox :: String -> Box
parseBox line =
  let [x, y, z] = map read $ splitOn "," line
    in V3 x y z

getResultPart1 :: Parsed -> Int
getResultPart1 = product . take 3 . getSizes . getGroups 1000

getResultPart2 :: Parsed -> Int
getResultPart2 = const 0

type Idx = Int
type BoxesWithDistance = [(Idx, Idx, Int)]

type Grouping = Array.Array Int Int

getSizes :: Grouping -> [Int]
getSizes = sortBy (comparing Down) . map length . group . sort . filter (/= 0) . Array.elems

type Steps = Int

getGroups :: Steps -> Boxes -> Grouping
getGroups steps boxes =
  let ordered = orderedDistances boxes
      firstSteps = take steps ordered
      initial = Array.listArray (1, length boxes) (repeat 0)
  in foldl (\arr (b1, b2, _) -> connect b1 b2 arr) initial firstSteps

connect :: Idx -> Idx -> Grouping -> Grouping
connect i j uf =
  let
    g1 = groupElems uf i
    g2 = groupElems uf j
    vi = uf Array.! i
    vj = uf Array.! j
    nxtValue
      | vi /= 0 = vi
      | vj /= 0 = vj
      | otherwise = maximum uf + 1
  in
    uf Array.// [(p, nxtValue) | p <- [1 .. rangeSize $ Array.bounds uf], p `elem` g1 || p `elem` g2]

groupElems :: Grouping -> Idx -> [Idx]
groupElems uf idx =
  let val = uf Array.! idx
  in if val == 0 then [idx] else [p | p <- [1 .. rangeSize $ Array.bounds uf], uf Array.! p == val ]

orderedDistances :: Boxes -> BoxesWithDistance
orderedDistances = sortBy (comparing (\(_, _, d) -> d)) . distances

distances :: Boxes -> BoxesWithDistance
distances boxes =
  let
    withIndex = zip [1..] boxes
  in
    [(i, j, qd bi bj) |
      (i, bi) <- withIndex,
      (j, bj) <- withIndex,
      i < j]