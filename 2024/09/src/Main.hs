import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List as L
import Data.List.Extra (splitOn)
import Data.Char
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Text.Regex.Applicative (RE, string, sym, (<|>))
import Text.Regex.Applicative.Common (decimal)

main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile. head =<< getArgs

type Input = Disk
type Disk = [Maybe FileId]
type FileId = Int
type Files = [Int]
type ContinuousDisk = [FilledDiskPos]
type FilledDiskPos = FileId

solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: String -> Input
parseEasy = concatMap (uncurry toSpace). zip [0..]. map digitToInt. trimEnd
    where
    trimEnd = reverse. (dropWhile (not. isDigit)). reverse 
    toSpace blockId n = replicate n (getPosType blockId)
    getPosType blockId = if even blockId then Just (blockId `div` 2) else Nothing

getResultEasy :: Input -> Int
getResultEasy disk = getScore. segmentDisk need files $ disk
    where
    files = getFilesFromEnd disk
    need = requiredSpace files

segmentDisk :: Int -> Files -> Disk -> ContinuousDisk
segmentDisk need [] _ = []
segmentDisk 0 _ _ = []
segmentDisk need fs (Just fileId:rest) = fileId : segmentDisk (need-1) fs rest
segmentDisk need (f:fs) (Nothing:rest) = f : segmentDisk (need-1) fs rest

getScore :: ContinuousDisk -> Int
getScore = sum. zipWith (*) [0..]

parseHard :: String -> Input
parseHard = parseEasy

getResultHard :: Input -> Int
getResultHard = const 0

getFilesFromEnd :: Disk -> Files
getFilesFromEnd = reverse. map (fromMaybe (error "?")). filter isJust

requiredSpace :: Files -> Int
requiredSpace = length
