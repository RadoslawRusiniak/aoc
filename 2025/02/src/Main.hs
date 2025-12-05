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
type Intervals = [Interval]

type Id = Int

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse = map parseInterval . splitOn (",")

parseInterval :: String -> Interval
parseInterval = toTuple . map read . splitOn ("-")
  where toTuple [x, y] = (x, y)

getResultPart1 :: Parsed -> Int
getResultPart1 = getResult 2

getResultPart2 :: Parsed -> Int
getResultPart2 intervals = getResult (maxDigits intervals) intervals
  where maxDigits = length . show . mxBound

type MaxNumberOfRepetitionsToTry = Int

getResult :: MaxNumberOfRepetitionsToTry -> Intervals -> Int
getResult maxRepetitions intervals = 
  sum . 
  getInvalidIds intervals .
  mergeWithoutDuplicates . 
  map (getRepeatedIds maximumValue) $
  [2 .. maxRepetitions]
  where
    maximumValue = mxBound intervals

getInvalidIds :: Intervals -> [Id] -> [Id]
getInvalidIds intervals = filter (flip inAnyInterval intervals)

mxBound :: Intervals -> Int
mxBound = maximum . map snd

inAnyInterval :: Id -> Intervals -> Bool
inAnyInterval x = any (inInterval x)

inInterval :: Id -> Interval -> Bool
inInterval x (left, right) = left <= x && x <= right

mergeWithoutDuplicates :: [[Int]] -> [Int]
mergeWithoutDuplicates = Set.toList . mconcat . map Set.fromList

getRepeatedIds :: Int -> Int -> [Id]
getRepeatedIds maxValue repeatFactor = takeWhile (<= maxValue) $ map (repeatInt repeatFactor) [1 ..]

repeatInt :: Int -> Int -> Int
repeatInt n = read . repeatString n . show

repeatString :: Int -> String -> String
repeatString n = concat . replicate n