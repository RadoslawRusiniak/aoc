import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List.Split
import Data.List as L
import Data.Char
import GHC.Utils.Misc (count, nTimes)
import qualified Data.Sequence as Sq
import Data.Foldable
import Data.List.Extra as LE
import Data.Maybe
import qualified Data.Map as Mp
import qualified Data.Set as St

main = putStr. solve =<< getFileContents
    where getFileContents = readFile. head =<< getArgs


type Cycle = Int
type XRegister = Int
type Accumulator = [Char]
type State = (Cycle, XRegister, Accumulator)
initState = (0, 1, [])

data Command = Noop | Addx Int deriving Show

type Result = String
solve :: String -> Result
solve = getResult. parse

parse :: String -> [Command]
parse = map toCommand. lines

toCommand :: String -> Command
toCommand = fromWords. words
    where
    fromWords [_, v] = Addx (read v)
    fromWords _ = Noop

getResult :: [Command] -> Result
getResult = getPixels. handleCommands initState
    where
    getPixels (_, _, r) = intercalate "\n". LE.chunksOf 40. LE.reverse $ r

handleCommands :: State -> [Command] -> State
handleCommands s = foldl (handleCommand) s

handleCommand :: State -> Command -> State
handleCommand (cycle, x, r) Noop = (newCycle, x, getPixelToDraw cycle x : r)
    where
    newCycle = cycle + 1
handleCommand (cycle, x, r) (Addx v) = (lastCycle, newX, pixels)
    where
    midCycle = cycle + 1
    lastCycle = cycle + 2
    newX = x + v
    pixels = getPixelToDraw midCycle x : getPixelToDraw cycle x : r

getPixelToDraw :: Cycle -> XRegister -> Char
getPixelToDraw c x = if x - 1 <= pos && pos <= x + 1 then '#' else '.'
    where
    pos = c `mod` 40
