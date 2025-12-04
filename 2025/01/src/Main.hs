import System.Environment (getArgs)

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = [Rotation]

type Rotation = Int

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse = map parseLine . lines
  where
    parseLine ('L':val) = (-1) * read val
    parseLine ('R':val) = read val

getResultPart1 :: Parsed -> Int
getResultPart1 = check

getResultPart2 :: Parsed -> Int
getResultPart2 = check2

check :: [Rotation] -> Int
check = length . filter isGood . scanl applyOp 50
  where
    isGood = (== 0)
    applyOp res rot = (res + rot) `mod` 100

check2 :: [Rotation] -> Int
check2 = sum . map snd . scanl (applyOp . fst) (50, 0)
  where
    applyOp pos rot = ((pos + rot) `mod` 100, getScore pos rot)

getScore :: Rotation -> Rotation -> Int    
getScore pos rot = abs (pos + rot) `div` 100 + if goesThroughZero then 1 else 0
  where goesThroughZero = signum pos /= 0 && signum pos /= signum (pos + rot)