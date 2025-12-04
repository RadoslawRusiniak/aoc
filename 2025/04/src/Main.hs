import System.Environment (getArgs)
import Data.Char (isSpace)
import Data.Array qualified as Array
import Data.List (unfoldr)
import Linear.V2 (V2(..))

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = Array.Array (V2 Int) Char

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse input = Array.listArray (V2 1 1, V2 rows columns) $ filter (not . isSpace) input
  where
    rows = length lined
    columns = length $ head lined
    lined = lines input

getResultPart1 :: Parsed -> Int
getResultPart1 = length . removableIndices
  
removableIndices :: Parsed -> [V2 Int]
removableIndices arr = [ ix | ix <- Array.indices arr, isPaper ix, reachable ix ]
  where
    isPaper ix = arr Array.! ix == '@'
    reachable ix = (< 4) . length . filter (== '@') $ neighbours ix arr
    
neighbours :: V2 Int -> Parsed -> [Char]
neighbours start@(V2 rw cl) arr = [ arr Array.! adj |
      x <- [rw-1 .. rw+1], y <- [cl-1 .. cl+1],
      let adj = V2 x y,
      start /= adj,
      Array.inRange (Array.bounds arr) adj]

getResultPart2 :: Parsed -> Int
getResultPart2 = sum . unfoldr step
  where
    step arr =
      let changes = removableIndices arr
      in if null changes
        then Nothing
        else Just (length changes, apply arr changes)
    apply arr idxs = arr Array.// [ (ix, '.') | ix <- idxs ]
