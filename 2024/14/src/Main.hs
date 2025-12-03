import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.List as List
import Data.List.Extra (splitOn)
import Data.Char
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Linear.V2 (V2(..))
import Text.Regex.Applicative (RE, string, sym, (<|>))
import Text.Regex.Applicative.Common (decimal)

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = [Robot]

part1, part2 :: Input -> Integer
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse = map parseRobot . lines

parseRobot :: String -> Robot
parseRobot = (\[p, v] -> (parseStart p, parseStep v)) . words

parseStart :: String -> Start
parseStart = parseV2

parseStep :: String -> Step
parseStep = parseV2

parseV2 :: String -> V2 Int
parseV2 (f:s:rest) = (\[x, y] -> V2 x y) . map read $ splitOn [','] rest

getResultPart1 :: Parsed -> Integer
getResultPart1 = getScore

getResultPart2 :: Parsed -> Integer
getResultPart2 = const 0

type Robot = (Start, Step)
type Start = Pos
type Step = V2 Int
type Pos = V2 Int

getScore :: [Robot] -> Integer
getScore = product . map toInteger . calculateQuadrants . filter anyQuadrant . map (toQuadrant . move)
  where
    anyQuadrant (x, y) = x /= 0 && y /= 0

calculateQuadrants :: [(Int, Int)] -> [Int]
calculateQuadrants xs = map snd . Map.toList $ Map.fromListWith (+) [(x, 1) | x <- xs]

toQuadrant :: Pos -> (Int, Int)
toQuadrant (V2 x y) = (check x limitWidth, check y limitHeight)
  where
    check a lim = orderingToInt (compare a mid)
      where
        mid = div lim 2
        orderingToInt LT = -1
        orderingToInt EQ = 0
        orderingToInt GT = 1

move :: Robot -> Pos
move (start, step) = normalizeV2 . (!! 100) $ iterate (moveStep step) start

moveStep :: Step -> Pos -> Pos
moveStep s p = s + p

normalizeV2 :: Pos -> Pos
normalizeV2 (V2 x y) = V2 (normalize limitWidth x) (normalize limitHeight y)

normalize :: Int -> Int -> Int
normalize limit v = mod v limit

limitWidth :: Int
limitWidth = 101

limitHeight :: Int
limitHeight = 103