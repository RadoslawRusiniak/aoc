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
getResultPart2 = product . getXs . goUntilAllConnected
  where
    getXs (v1, v2) = [x | V3 x _ _ <- [v1, v2]]

type Idx = Int
type BoxesWithDistance = [((Idx, Box), (Idx, Box), Int)]

type Grouping = Array.Array Int Int

getSizes :: Grouping -> [Int]
getSizes = sortBy (comparing Down) . map length . group . sort . filter (/= 0) . Array.elems

type Steps = Int

getGroups :: Steps -> Boxes -> Grouping
getGroups steps boxes =
  let ordered = orderedDistances boxes
      firstSteps = take steps ordered
      initial = Array.listArray (1, length boxes) (repeat 0)
  in foldl (\arr ((i, _), (j, _), _) -> connect i j arr) initial firstSteps

goUntilAllConnected :: Boxes -> (Box, Box)
goUntilAllConnected boxes =
  let 
    ordered = orderedDistances boxes
    initial = Array.listArray (1, length boxes) (repeat 0)
  in
    goWithState initial ordered

goWithState :: Grouping -> BoxesWithDistance -> (Box, Box)
goWithState _ [] = error "Not all connected"
goWithState uf (((i, bi), (j, bj), _) : rest) =
  let 
    newUf = connect i j uf
    isAllConnected arr = notElem 0 $ Array.elems arr
  in 
    if isAllConnected newUf
      then (bi, bj)
      else goWithState newUf rest

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
    [((i, bi), (j, bj), qd bi bj) |
      (i, bi) <- withIndex,
      (j, bj) <- withIndex,
      i < j]