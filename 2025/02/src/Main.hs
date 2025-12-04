import System.Environment (getArgs)
import Data.List.Extra (splitOn)
import Data.Set qualified as Set (fromList, toList)

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
getResultPart1 intervals = sum . getInvalidIds intervals . map (repeatInt 2) $ [1 ..]

inAnyInterval :: [Interval] -> Int -> Bool
inAnyInterval intervals x = any (inInterval x) intervals

getInvalidIds :: [Interval] -> [Int] -> [Int]
getInvalidIds intervals = filter (inAnyInterval intervals) . takeWhile (<= mxBound)
  where
    mxBound = maximum $ map snd intervals

inInterval :: Int -> Interval -> Bool
inInterval x (left, right) = left <= x && x <= right

getResultPart2 :: Parsed -> Int
getResultPart2 intervals = sum . mergeElements . map (getInvalidIds intervals . getNumbersForRepetition) $ [2 .. 11]

mergeElements :: [[Int]] -> [Int]
mergeElements = Set.toList . mconcat . map Set.fromList

getNumbersForRepetition :: Int -> [Int]
getNumbersForRepetition n = map (repeatInt n) [1 ..]

repeatInt :: Int -> Int -> Int
repeatInt n = read . repeatString n . show

repeatString :: Int -> String -> String
repeatString n = mconcat . replicate n