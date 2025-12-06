import System.Environment (getArgs)

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = String

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse = id

getResultPart1 :: Parsed -> Int
getResultPart1 = const 0

getResultPart2 :: Parsed -> Int
getResultPart2 = const 0
