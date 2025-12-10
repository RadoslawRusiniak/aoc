import System.Environment (getArgs)
import Data.Array qualified as Array
import Data.List.Split (splitOn)

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = [State]

type State = (DesiredLightsState, [Wiring])
type DesiredLightsState = LightsState
type LightsState = Array.Array Int Bool
type Wiring = [Int]

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse = map parseState . lines

parseState :: String -> State
parseState line = (parseLightsState, parseWirings)
  where
    worded = words line
    parseLightsState = (\ls -> Array.listArray (0, length ls - 1) ls) . map (== '#') . dropParens . head $ worded
    parseWirings = map parseWiring . init . tail $ worded
    parseWiring = map read . splitOn "," . dropParens
    dropParens = init . tail


getResultPart1 :: Parsed -> Int
getResultPart1 = sum . map getMinimumOps
  
getMinimumOps :: State -> Int
getMinimumOps (endState, wirings) = minimum . map length . filter isGood $ powerset wirings
  where
    isGood ws = applyOps (Array.rangeSize (Array.bounds endState)) ws == endState

getResultPart2 :: Parsed -> Int
getResultPart2 = const 0

type Size = Int

applyOps :: Size -> [Wiring] -> LightsState
applyOps sz = foldl applyOp initState
  where
    initState = Array.listArray (0, sz - 1) (replicate sz False)

applyOp :: LightsState -> Wiring -> LightsState
applyOp ls wiring = ls Array.// updates
  where
    updates = map (\i -> (i, not (ls Array.! i))) wiring

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = let ps = powerset xs
                  in ps ++ map (x:) ps
