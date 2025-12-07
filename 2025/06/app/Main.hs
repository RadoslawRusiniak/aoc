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
type Operations = [Operation]
type Operation = (Int -> Int -> Int)

part1, part2 :: Input -> Int
part1 = getResultPart1 . parsePart1
part2 = getResultPart2 . parsePart2

parsePart1 :: Input -> Parsed
parsePart1 = parseToNumbersAndOperations parseNumbers

parsePart2 :: Input -> Parsed
parsePart2 = parseToNumbersAndOperations parseVerticalNumbers

type NumbersParser = [String] -> NumbersMatrix

parseToNumbersAndOperations :: NumbersParser -> Input -> Parsed
parseToNumbersAndOperations numbersParser input =
  let lined = lines input
  in (numbersParser (init lined), parseOperations (last lined))

parseNumbers :: [String] -> [[Int]]
parseNumbers = transpose . map parseLine
  where
    parseLine = map read . words

parseVerticalNumbers :: [String] -> NumbersMatrix
parseVerticalNumbers = 
  map readGroup .
  splitWhen isBlankLine .
  transpose .
  map init -- needed since "lines" from "parsePart2" only removes '\n' at the end of lines, so I get '\r' at the end of each line
  where
    isBlankLine = all isSpace
    readGroup = map read

parseOperations :: String -> Operations
parseOperations = map (\w -> if w == "+" then (+) else (*)) . words

getResultPart1 :: Parsed -> Int
getResultPart1 = sum . map (uncurry $ flip foldr1) . uncurry zip

getResultPart2 :: Parsed -> Int
getResultPart2 = getResultPart1