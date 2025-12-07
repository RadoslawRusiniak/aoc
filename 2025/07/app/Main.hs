import System.Environment (getArgs)
import Data.Array qualified as Array
import Data.Char (isSpace)
import Data.Set qualified as Set
import Linear.V2 (V2(..))

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = Array.Array (V2 Int) Char
type Pos = V2 Int

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse input = 
  let
    lined = map (filter (not . isSpace)) $ lines input
    rows = length lined
    columns = length $ head lined
  in
    Array.listArray (V2 1 1, V2 rows columns) $ filter (not . isSpace) input

getResultPart1 :: Parsed -> Int
getResultPart1 = Set.size . getUsedBeamSplits

getUsedBeamSplits :: Parsed -> Set.Set Pos
getUsedBeamSplits arr = fst . foldl go (Set.empty, Set.empty) . Array.indices $ arr
  where
    go :: (Set.Set Pos, Set.Set Pos) -> V2 Int -> (Set.Set Pos, Set.Set Pos)
    go (usedBeams, usedPos) idx@(V2 x y) = 
      case arr Array.! idx of
        'S' -> (usedBeams, Set.insert idx usedPos)
        '.' -> (usedBeams, addIfUp usedPos idx [idx])
        '^' -> (addIfUp usedBeams idx [idx], addIfUp usedPos idx [V2 x (y-1), V2 x (y+1)])
      where
        addIfUp st curPos positionsToAdd =
          if isUp curPos then
            Set.union st (Set.fromList positionsToAdd)
          else
            st
        isUp (V2 x y) = Set.member (V2 (x-1) y) usedPos 

getResultPart2 :: Parsed -> Int
getResultPart2 = const 0
