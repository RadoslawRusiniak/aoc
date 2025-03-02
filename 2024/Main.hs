import System.Environment (getArgs)
import qualified Data.Map as Map

main :: IO ()
main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile . head =<< getArgs

type ParsedInput = [Int]
type Freq = Map.Map Int Int

solveEasy, solveHard :: String -> Int
solveEasy = getResultEasy . parseEasy
solveHard = getResultHard . parseHard

parseEasy :: String -> ParsedInput
parseEasy = map read . words

getResultEasy :: ParsedInput -> Int
getResultEasy = getScore 25

parseHard :: String -> ParsedInput
parseHard = parseEasy

getResultHard :: ParsedInput -> Int
getResultHard = getScore 75

getScore :: Int -> [Int] -> Int
getScore runs = toScore . (!! runs) . iterate blink . toMap
    where
    toMap = Map.fromListWith (+) . map (,1)
    toScore = sum . Map.elems

blink :: Freq -> Freq
blink = Map.fromListWith (+) . concatMap toSeparate . map runChange . Map.assocs
    where
    runChange (stone, cnt) = (change stone, cnt)
    toSeparate (stones, cnt) = map (,cnt) stones

change :: Int -> [Int]
change 0 = [1]
change n
    | even len = (\(a, b) -> [read a, read b]) . splitAt (len `div` 2) $ s
    | otherwise = [n * 2024]
    where
    s = show n
    len = length s