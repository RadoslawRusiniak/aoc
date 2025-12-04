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
getResultPart1 = getResult 2

inAnyInterval :: [Interval] -> Int -> Bool
inAnyInterval intervals x = any (inInterval x) intervals

getInvalidIds :: [Interval] -> [Int] -> [Int]
getInvalidIds intervals = filter (inAnyInterval intervals) . takeWhile (<= maximumValue)
  where maximumValue = mxBound intervals
    
mxBound :: [Interval] -> Int
mxBound = maximum . map snd

inInterval :: Int -> Interval -> Bool
inInterval x (left, right) = left <= x && x <= right

getResultPart2 :: Parsed -> Int
getResultPart2 intervals = getResult (mxDigits intervals) intervals
  where
    mxDigits = length . show . mxBound

type MaxNumberOfRepetitionsToTry = Int

getResult :: MaxNumberOfRepetitionsToTry -> [Interval]  -> Int
getResult n intervals = sum . eliminateDuplicates . map (getInvalidIds intervals . getRepeatedNumbers) $ [2 .. n]

eliminateDuplicates :: [[Int]] -> [Int]
eliminateDuplicates = Set.toList . mconcat . map Set.fromList

getRepeatedNumbers :: Int -> [Int]
getRepeatedNumbers n = map (repeatInt n) [1 ..]

repeatInt :: Int -> Int -> Int
repeatInt n = read . repeatString n . show

repeatString :: Int -> String -> String
repeatString n = mconcat . replicate n