import System.Environment (getArgs)
import Data.List (transpose)

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
data Operation = Mul | Add

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse = (\xs -> (parseNumbers (init xs), parseOperations (last xs))) . lines

parseNumbers :: [String] -> [[Int]]
parseNumbers = transpose . map parseLine
  where
    parseLine = map read . words
    
parseOperations :: String -> Operations
parseOperations = map (\w -> if w == "+" then Add else Mul) . words

getResultPart1 :: Parsed -> Int
getResultPart1 (numbersMatrix, operations) = sum . map (uncurry applyOp) $ zip operations numbersMatrix

applyOp :: (Operation -> Numbers -> Int)
applyOp Add = foldl1 (+)
applyOp Mul = foldl1 (*)

getResultPart2 :: Parsed -> Int
getResultPart2 = const 0