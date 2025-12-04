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
getResultPart1 = getScore (getMax 2)

getResultPart2 :: Parsed -> Int
getResultPart2 = getScore (getMax 12)

getScore :: ([Int] -> Int) -> [[Int]] -> Int
getScore f = sum . map f

getMax :: Int -> [Int] -> Int
getMax 1 arr = maximum arr
getMax n arr = maxVal * 10^n + getMax (n-1) (drop (maxPos + 1) arr)
  where 
    (maxPos, maxVal) = maxWithIndex curArr
    curArr = take (length arr - (n-1)) arr

maxWithIndex :: [Int] -> (Int, Int)
maxWithIndex = maximumBy (comparing snd <> comparing (Down . fst)) . zip [0..]