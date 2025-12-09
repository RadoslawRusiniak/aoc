import System.Environment (getArgs)
import qualified Data.Array as Array
import Data.List.Split (splitOn)
import Linear.V2 (V2(..))
import Data.List (maximumBy)

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = Positions

type Positions = [Pos]
type Pos = V2 Int

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse = map parsePos . lines

parsePos :: String -> Pos
parsePos line = let [x, y] = map read $ splitOn "," line
                in  V2 x y

getResultPart1 :: Parsed -> Int
getResultPart1 = getBiggestRectangle

getResultPart2 :: Parsed -> Int
getResultPart2 = const 0

getBiggestRectangle :: [Pos] -> Int
getBiggestRectangle vs = maximum [rectangleSize v1 v2 | (i, v1) <- withIndex, (j, v2) <- withIndex, i < j]
  where withIndex = zip [0..] vs
    
rectangleSize :: Pos -> Pos -> Int    
rectangleSize (V2 x1 y1) (V2 x2 y2) = side x1 x2 * side y1 y2
  where side a1 a2 = abs (a2 - a1) + 1
