import System.Environment (getArgs)
import Data.List.Extra (splitOn)

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = [Interval]

type Interval = (Int, Int)

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse = map parseInterval . splitOn (",")

parseInterval :: String -> Interval
parseInterval s = (fstInt, sndInt)
  where
    fstInt = read . takeWhile (/= '-') $ s
    sndInt = read . tail . dropWhile (/= '-') $ s

getResultPart1 :: Parsed -> Int
getResultPart1 intervals = sum . filter (inAnyInterval intervals) . takeWhile (<= mxBound) . map doubleUp $ [1 ..]
  where
    mxBound = maximum $ map snd intervals

doubleUp :: Int -> Int
doubleUp = read . (\s -> s ++ s) . show

inAnyInterval :: [Interval] -> Int -> Bool
inAnyInterval intervals x = any (inInterval x) intervals

inInterval :: Int -> Interval -> Bool
inInterval x (left, right) = left <= x && x <= right

getResultPart2 :: Parsed -> Int
getResultPart2 = const 0
