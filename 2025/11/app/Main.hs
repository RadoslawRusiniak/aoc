import System.Environment (getArgs)
import Data.Map.Strict qualified as Map

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  print . part1 $ file
  print . part2 $ file

type Input = String
type Parsed = Graph

type Vertex = String
type Graph = Map.Map Vertex [Vertex]

part1, part2 :: Input -> Int
part1 = getResultPart1 . parse
part2 = getResultPart2 . parse

parse :: Input -> Parsed
parse = Map.fromList . map parseEdge . lines

parseEdge :: String -> (Vertex, [Vertex])
parseEdge line =
  let
    worded = words line
    name = init . head $ worded
    neighbours = tail worded
    isOut = neighbours == ["out"]
  in
    (name, if isOut then [] else neighbours)

getResultPart1 :: Parsed -> Int
getResultPart1 = flip getPaths "you"

getResultPart2 :: Parsed -> Int
getResultPart2 = const 0

getPaths :: Graph -> Vertex -> Int
getPaths graph = go
  where
    go v =
      case Map.lookup v graph of
        Nothing -> 0
        Just [] -> 1
        Just neighbours -> sum $ map go neighbours
