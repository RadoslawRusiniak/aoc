import System.Environment (getArgs)
import Data.List as L
import Data.Char
import Data.Maybe

main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile. head =<< getArgs

type Input = Disk
type Disk = [Block]
data Block = Block {
    idx :: Maybe Index
    , len :: Length
    } deriving (Show, Eq)
type Index = Int
type Length = Int

solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: String -> Input
parseEasy = toDisk. map digitToInt. takeWhile isDigit

parseHard :: String -> Input
parseHard = parseEasy

toDisk :: [Int] -> [Block]
toDisk = map (uncurry Block). zip indexation
    where
    indexation = intersperse Nothing. map Just $ [0..]

getResultEasy :: Input -> Int
getResultEasy = getScore. refragmentDisk greedyInsert

getResultHard :: Input -> Int
getResultHard = getScore. refragmentDisk wholeInsert

refragmentDisk :: InsertAlgo -> Disk -> Disk
refragmentDisk algo disk = foldr refragmentFile disk (files disk)
    where
    refragmentFile file = 
        concat. snd. mapAccumL (insertFile algo) (Just file). removeFile file
    removeFile f@(Block _ l) (x:xs)
        | f == x    = (Block Nothing l) : xs
        | otherwise = x : removeFile f xs

type LeftToInsert = Maybe Block
type ResultingBlocks = [Block]
type Result = (LeftToInsert, ResultingBlocks)
type InsertAlgo = Index -> Need -> Space -> Result
type BlockToInsert = Maybe Block
type ComparedBlock = Block
insertFile :: InsertAlgo -> BlockToInsert -> ComparedBlock -> Result
insertFile _ Nothing b = (Nothing, [b])
insertFile _ toInsert b@(Block (Just _) _) = (toInsert, [b])
insertFile algo (Just (Block (Just idx) need)) (Block _ space) = 
    algo idx need space

type Need = Int
type Space = Int
greedyInsert :: InsertAlgo
greedyInsert idx need space = (toInsertBlock, insertedBlock : leftover)
    where
    insertedLen = min space need
    leftSpace = space - insertedLen
    toInsertBlock = if insertedLen < need
        then Just (Block (Just idx) (need - insertedLen))
        else Nothing
    insertedBlock = Block (Just idx) insertedLen 
    leftover = if leftSpace > 0
        then [Block Nothing leftSpace]
        else []

wholeInsert :: InsertAlgo
wholeInsert idx need space
    | need > space = (Just (Block (Just idx) need), [Block Nothing space])
    | otherwise = (Nothing, (Block (Just idx) need) : leftover)
        where
        leftover = if need < space
            then [Block Nothing (space - need)]
            else []

getScore :: Disk -> Int
getScore = fst. foldl' go (0, 0)
    where
    go (score, pos) (Block idx len) = (score + calculateForBlock, nextPos)
        where 
        nextPos = pos + len
        calculateForBlock = (fromMaybe 0 idx) * sum [pos .. nextPos - 1] 

files :: Disk -> [Block]
files = filter (isJust. idx)