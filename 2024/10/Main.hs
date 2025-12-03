import System.Environment (getArgs)
import Data.Array qualified as Array
import Data.Char
import Data.Set qualified as Set
import Linear.V2 (V2(..))
import Linear.Vector (basis)

main :: IO ()
main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile . head =<< getArgs

type Idx = V2 Int
type Bounds = (Idx, Idx)
type Arr = Array.Array Idx Int
type ParsedInput = Arr

solveEasy, solveHard :: String -> Int
solveEasy = getResultEasy . parseEasy
solveHard = getResultHard . parseHard

parseEasy :: String -> ParsedInput
parseEasy str = createArray . map digitToInt . filter isDigit $ str
    where
    createArray = Array.listArray (V2 1 1, V2 rows columns)
    rows = length lined
    columns = length $ head lined
    lined = lines str

getResultEasy :: ParsedInput -> Int
getResultEasy arr = getResult (length . uniqueReachableSummits arr) arr

type Scoring = Idx -> Int

getResult :: Scoring -> ParsedInput -> Int
getResult scoring = sum . map scoring . getStarts
    where
    getStarts = map fst . filter ((== 0) . snd) . Array.assocs

uniqueReachableSummits :: Arr -> Idx -> [Idx]
uniqueReachableSummits arr = Set.toList . Set.fromList . reachableSummits arr

reachableSummits :: Arr -> Idx -> [Idx]
reachableSummits arr idx
    | arr Array.! idx == 9 = [idx]
    | otherwise = concatMap (reachableSummits arr) . reachableNeighbours arr $ idx

reachableNeighbours :: Arr -> Idx -> [Idx]
reachableNeighbours arr idx = filter (isHigher idx) . validNeighbours (Array.bounds arr) $ idx
    where
    isHigher cur nxt = (arr Array.! cur) + 1 == arr Array.! nxt

validNeighbours :: Bounds -> Idx -> [Idx]
validNeighbours bnds = filter (Array.inRange bnds). allNeighbours
    where
    allNeighbours idx = map (idx +) directions
    directions = [id, negate] <*> basis

parseHard :: String -> ParsedInput
parseHard = parseEasy

getResultHard :: ParsedInput -> Int
getResultHard arr = getResult (length. reachableSummits arr) arr