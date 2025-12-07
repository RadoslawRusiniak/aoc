import System.Environment (getArgs)
import Control.Lens ((^.))
import Data.Array qualified as Array
import Data.Char (isSpace)
import Linear.V2 (V2(..), _y)

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = Array.Array Pos Char
type Pos = V2 Int

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse input = 
  let
    lined = map (filter (not . isSpace)) $ lines input -- filter necessary because of Windows line endings - "lines" cuts only '\n', leaves '\r'
    rows = length lined
    columns = length $ head lined
  in
    Array.listArray (V2 1 1, V2 rows columns) $ filter (not . isSpace) input

getResultPart1 :: Parsed -> Int
getResultPart1 = fst . getUsedBeamsAndRoads

getResultPart2 :: Parsed -> Int
getResultPart2 = sum . Array.elems . snd . getUsedBeamsAndRoads

type UsedBeamsCount = Int
type WaysCount = Int
type Column = Int
type State = (UsedBeamsCount, Array.Array Column WaysCount)

getUsedBeamsAndRoads :: Parsed -> State
getUsedBeamsAndRoads arr =
  let
    startColumn = getColumn $ getStart arr
    orderedBeamsColumns = map getColumn $ getBeams arr
    initState = Array.array (1, maxCol) [(idx, val) | idx <- [1..maxCol], let val = if idx == startColumn then 1 else 0]
    maxCol = getColumn . snd $ Array.bounds arr
  in
    foldl applyBeam (0, initState) orderedBeamsColumns

applyBeam :: State -> Column -> State
applyBeam state@(usedBeams, ways) beamColumn =
  let
    curValue = ways Array.! beamColumn
    curLeft = ways Array.! (beamColumn - 1)
    curRight = ways Array.! (beamColumn + 1)
    waysAfterBeam = ways Array.// [(beamColumn, 0), (beamColumn - 1, curLeft + curValue), (beamColumn + 1, curRight + curValue)]
  in
    if curValue == 0 then state else (usedBeams + 1, waysAfterBeam)

getStart :: Parsed -> Pos
getStart = head . getForSymbol 'S'

getBeams :: Parsed -> [Pos]
getBeams = getForSymbol '^'

getForSymbol :: Char -> Parsed -> [Pos]
getForSymbol s = map fst . filter ((== s) . snd) . Array.assocs

getColumn :: Pos -> Column
getColumn = (^. _y)