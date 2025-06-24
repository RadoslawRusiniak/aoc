import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.Array
import Data.List as List
import Data.List.Extra (splitOn)
import Data.Char
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Linear ((*^))
import Linear.V2 (V2(..))
import Linear.Matrix (M22(..), (!*))
import Text.Regex.Applicative (RE, string, sym, (<|>))
import Text.Regex.Applicative.Common (decimal)

main :: IO ()
main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile. head =<< getArgs

type Input = String
type Parsed = [Machine]

solveEasy, solveHard :: Input -> Int
solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: Input -> Parsed
parseEasy = parseMachines

getResultEasy :: Parsed -> Int
getResultEasy = machinesScore

parseHard :: Input -> Parsed
parseHard = parseEasy

getResultHard :: Parsed -> Int
getResultHard = sum . mapMaybe (getScoreHard . toHard)
  where
    toHard (g, bA, bB) = ((+10000000000000) <$> g, bA, bB)

getScoreHard :: Machine -> Maybe Int
getScoreHard (g, V2 x1 y1, V2 x2 y2) = do
  let (det, u) = inv22det (V2 (V2 x1 x2) (V2 y1 y2))
  guard $ det /= 0
  let ug = u !* g
  guard $ all ((== 0) . (`mod` det)) ug
  let V2 a b = (`div` det) <$> ug
  return $ 3 * a + b

inv22det :: M22 Int -> (Int, M22 Int)
inv22det (V2 (V2 a b) (V2 c d)) = (a*d - b*c, V2 (V2 d (-b)) (V2 (-c) a))

parseMachines :: Input -> Parsed
parseMachines = map parseMachine . splitOn [""] . lines

parseMachine :: [String] -> Machine
parseMachine [line1, line2, line3] = (parseGoal line3, parseButton line1, parseButton line2)

parseGoal :: String -> Goal
parseGoal = (\[_, x, y] -> V2 (toVal x) (toVal y)) . words

parseButton :: String -> Goal
parseButton = (\[_, _, x, y] -> V2 (toVal x) (toVal y)) . words 

toVal :: String -> Int
toVal = read . filter isDigit

type Button = V2 Int
type ButtonA = Button
type ButtonB = Button
type Goal = V2 Int
type ClawTry = (Int, Button)
type TwoClawTry = (ClawTry, ClawTry)
type Machine = (Goal, ButtonA, ButtonB)

machinesScore :: [Machine] -> Int
machinesScore = sum . map machineScore

machineScore :: Machine -> Int
machineScore = minScore . allValid

minScore :: [TwoClawTry] -> Int
minScore = minimumDef 0 . map toScore

toScore :: TwoClawTry -> Int
toScore ((t1, _), (t2, _)) = t1 * 3 + t2

allValid :: Machine -> [TwoClawTry]
allValid (g, bA, bB) = filter (canReach g) $ allConfigsForTwo bA bB

allConfigsForTwo :: Button -> Button -> [TwoClawTry]
allConfigsForTwo bA bB = [(c1, c2) | c1 <- allConfigsForOne bA, c2 <- allConfigsForOne bB]

allConfigsForOne :: Button -> [ClawTry]
allConfigsForOne b = [(c, b) | c <- [0 .. 100]]

canReach :: Goal -> TwoClawTry -> Bool
canReach g (c1, c2) = g == clawReach c1 + clawReach c2
  where
    clawReach (t, s) = t *^ s

minimumDef :: Ord a => a -> [a] -> a
minimumDef def [] = def
minimumDef _   xs = minimum xs