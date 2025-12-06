import System.Environment (getArgs)
import Data.Char (isSpace)
import Data.List (transpose)
import Data.List.Split (splitWhen)

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = (NumbersMatrix, Operations)

type NumbersMatrix = [[Int]]
type Numbers = [Int]
type Operations = [Operation]
data Operation = Mul | Add deriving (Show)

part1, part2 :: Input -> Int
part1 = getResultPart1 . parsePart1
part2 = getResultPart2 . parsePart2

parsePart1 :: Input -> Parsed
parsePart1 = (\xs -> (parseNumbers (init xs), parseOperations (last xs))) . lines

parseNumbers :: [String] -> [[Int]]
parseNumbers = transpose . map parseLine
  where
    parseLine = map read . words
    
parseOperations :: String -> Operations
parseOperations = map (\w -> if w == "+" then Add else Mul) . words

parsePart2 :: Input -> Parsed
parsePart2 input = (numbers input, ops input)
  where
    numbers = parseVertNumbers . init . lines
    ops = parseOperations . last . lines

parseVertNumbers :: [String] -> NumbersMatrix
parseVertNumbers = map readDigitNumbers . splitWhen isBlankLine . init . transpose
  where
    isBlankLine = all isSpace
    readDigitNumbers = map readDigitNumber
    readDigitNumber = read . trim

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where dropWhileEnd f = reverse . dropWhile f . reverse

getResultPart1 :: Parsed -> Int
getResultPart1 (numbersMatrix, operations) = sum . map (uncurry applyOp) $ zip operations numbersMatrix

applyOp :: (Operation -> Numbers -> Int)
applyOp Add = foldl1 (+)
applyOp Mul = foldl1 (*)

getResultPart2 :: Parsed -> Int
getResultPart2 = getResultPart1