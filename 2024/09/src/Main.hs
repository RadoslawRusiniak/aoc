import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List as L
import Data.Char
import Data.Foldable
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

toDisk :: [Int] -> [Block]
toDisk = map (uncurry Block). zip indexation
    where
    indexation = intersperse Nothing. map Just $ [0..]

getResultEasy :: Input -> Int
getResultEasy = getScore. refragmentDisk

refragmentDisk :: Disk -> Disk
refragmentDisk disk = foldr refragmentFile disk (files disk)
    where
    refragmentFile file = 
        concat. snd. mapAccumL greedyInsert (Just file). removeFile file
    removeFile f@(Block _ l) (x:xs)
        | f == x    = (Block Nothing l) : xs
        | otherwise = x : removeFile f xs

type BlockToInsert = Maybe Block
type ComparedBlock = Block
type LeftToInsert = Maybe Block
type ResultingBlocks = [Block]
greedyInsert :: BlockToInsert -> ComparedBlock -> (LeftToInsert, ResultingBlocks)
greedyInsert Nothing b = (Nothing, [b])
greedyInsert toInsert b@(Block (Just _) _) = (toInsert, [b])
greedyInsert (Just (Block (Just idx) need)) (Block _ space) =
    (toInsertBlock, insertedBlock : leftover)
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

parseHard :: String -> Input
parseHard = parseEasy

getResultHard :: Input -> Int
getResultHard = const 0

getScore :: Disk -> Int
getScore = fst. foldl go (0, 0)
    where
    go (score, pos) (Block idx len) = (score + calculateForBlock, nextPos)
        where 
        nextPos = pos + len
        calculateForBlock = (fromMaybe 0 idx) * sum [pos .. nextPos - 1] 

files :: Disk -> [Block]
files = filter (isJust. idx)