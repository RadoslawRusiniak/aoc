import System.Environment (getArgs)
import Data.Char (digitToInt)
import Data.List (maximumBy)
import Data.Ord (Down(..), comparing)

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = [[Int]]

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse = map parseLine . lines
  where
    parseLine = map digitToInt

getResultPart1 :: Parsed -> Int
getResultPart1 = sum . map getMax

getResultPart2 :: Parsed -> Int
getResultPart2 = const 0

getMax :: [Int] -> Int
getMax arr = maxVal * 10 + (getSecondVal maxPos arr)
  where
    (maxPos, maxVal) = maxWithIndex (init arr)
    getSecondVal maxPos = maximum . drop (maxPos + 1)

maxWithIndex :: [Int] -> (Int, Int)
maxWithIndex = maximumBy (comparing snd <> comparing (Down . fst)) . zip [0..]