import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List as L
import Data.Char
import GHC.Utils.Misc (count, nTimes)
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.List.Extra as LE
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

main = print. solve =<< getFileContents
    where getFileContents = readFile. head =<< getArgs


type WorryLevel = Int
type Item = WorryLevel
type Op = WorryLevel -> WorryLevel
data Test = Test { testNumber :: Int, ifTrue :: Int, ifFalse :: Int } deriving (Show)
data Monkey = Monkey { items :: Seq.Seq WorryLevel, op :: Op, test :: Test, counter :: Int }

type ModNumber = Int
type Monkeys = Seq.Seq Monkey
type State = (Monkeys, ModNumber)

type Result = Integer
solve :: String -> Result
solve = getResult. parse

getResult :: State -> Result
getResult = getMostActive. toCounters. processNTimes 10000
    where
    toCounters = map counter. toList. fst
    getMostActive = product. map toInteger. take 2. reverse. sort

parse :: String -> State
parse input = (getSeqMonkeys. getMonkeys $ input, getModNumber. getMonkeys $ input)
    where
    getMonkeys = map parseMonkey. LE.splitOn [""]. lines
    getSeqMonkeys = Seq.fromList
    getModNumber = product. map (testNumber. test)

parseMonkey :: [String] -> Monkey
parseMonkey (_: itemsLn :opLn: testLns) = 
    Monkey (parseItems itemsLn) (parseOp opLn) (parseTest testLns) 0

parseItems :: String -> Seq.Seq Item
parseItems = Seq.fromList. map (read. LE.trim). LE.splitOn ",". dropWhile (not. isDigit)

parseOp :: String -> Op
parseOp = getOp. drop 4. words
    where
    getOp ["*", "old"] = getOpOld (*)
    getOp ["+", "old"] = getOpOld (+)
    getOp ["+", x] = getOp' (+) x
    getOp ["*", x] = getOp' (*) x
    getOpOld f = \x -> fromInteger (f (toInteger x) (toInteger x))
    getOp' f v = \x -> f x (read v)

parseTest :: [String] -> Test
parseTest [test, ifTrue, ifFalse] = Test (getTestNumber test) (getMonkeyIndex ifTrue) (getMonkeyIndex ifFalse)
    where
    getTestNumber = read. (!! 3). words
    getMonkeyIndex = read. (!! 5). words

processNTimes :: Int -> State -> State
processNTimes n s = nTimes n (processMonkeys) s

processMonkeys :: State -> State
processMonkeys st@(sq, _) = foldl processMonkey st [0 .. slen - 1]
    where
    slen = Seq.length sq

type Index = Int
processMonkey :: State -> Index -> State
processMonkey st@(sq, md) i = oldMonkeyUpdated. foldl' (processItem monkey) st $ monkeyItems
    where 
    monkey = Seq.index sq i
    monkeyItems = items monkey
    oldMonkeyWithoutItems = monkey { items = Seq.empty, counter = counter monkey + length monkeyItems }
    oldMonkeyUpdated (sq, md) = (Seq.update i oldMonkeyWithoutItems $ sq, md)

processItem :: Monkey -> State -> Item -> State
processItem m st@(sq, md) it = (updatedState $ sq, md)
    where
    newWorryLevel = getNewWorryLevel it (op m) md
    newMonkeyIndex = runTest (test m) newWorryLevel
    newMonkey = Seq.index sq newMonkeyIndex
    newMonkeyUpdatedItems = (items newMonkey) Seq.|> newWorryLevel
    updatedNewMonkey = newMonkey { items = newMonkeyUpdatedItems }
    updatedState = Seq.update newMonkeyIndex updatedNewMonkey

runTest :: Test -> WorryLevel -> Int
runTest t w = if w `mod` (testNumber t) == 0 then ifTrue t else ifFalse t

getNewWorryLevel :: WorryLevel -> Op -> ModNumber -> WorryLevel
getNewWorryLevel w o m = o w `mod` m
