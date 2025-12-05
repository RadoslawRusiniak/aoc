import System.Environment (getArgs)
import Data.List.Extra (splitOn, sort)
import Data.Set qualified as Set (empty, fromList, toList)

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = (Intervals, Ids)

type Interval = (Int, Int)
type Intervals = [Interval]

type Id = Int
type Ids = [Int]

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse input =
  let (intervals, _ : ids) = break (== "\r") $ lines input
  in (parseIntervals intervals, parseIds ids)

parseIntervals :: [String] -> Intervals
parseIntervals = map parseInterval

parseInterval :: String -> Interval
parseInterval = toTuple . map read . splitOn ("-")
  where toTuple [x, y] = (x, y)

parseIds :: [String] -> Ids
parseIds = map read

getResultPart1 :: Parsed -> Int
getResultPart1 (intervals, ids) = length $ getFreshIds intervals ids

getResultPart2 :: Parsed -> Int
getResultPart2 (intervals, _) = sum . map rangeSize $ mergeRanges intervals

getFreshIds :: Intervals -> Ids -> Ids
getFreshIds intervals = filter (flip inAnyInterval intervals)

inAnyInterval :: Id -> Intervals -> Bool
inAnyInterval x = any (inInterval x)

inInterval :: Id -> Interval -> Bool
inInterval x (left, right) = left <= x && x <= right

mergeRanges :: Intervals -> Intervals
mergeRanges = mergeSortedRanges . sort

mergeSortedRanges :: Intervals -> Intervals
mergeSortedRanges [] = []
mergeSortedRanges [r] = [r]
mergeSortedRanges (left@(left1, right1) : right@(left2, right2) : rest) =
  if (left2 <= right1) then
    mergeSortedRanges ((left1, max right1 right2) : rest)
  else
    left : mergeSortedRanges (right : rest)

rangeSize :: Interval -> Int
rangeSize (left, right) = right - left + 1