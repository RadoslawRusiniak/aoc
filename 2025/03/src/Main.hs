import System.Environment (getArgs)
import Data.Char (digitToInt, intToDigit)
import Data.List (maximumBy, unfoldr)
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
getMax n arr = digitsToInt (maxDigits n arr)

maxDigits :: Int -> [Int] -> [Int]
maxDigits n arr = unfoldr step (n, arr)
  where
    step (0, _) = Nothing
    step (m, xs) =
      let (pos,val) = maxWithIndex (take (length xs - (m-1)) xs)
      in Just (val, (m-1, drop (pos+1) xs))

maxWithIndex :: [Int] -> (Int, Int)
maxWithIndex = maximumBy (comparing snd <> comparing (Down . fst)) . zip [0..]

digitsToInt :: [Int] -> Int
digitsToInt ds = read (map intToDigit ds) :: Int
