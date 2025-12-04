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
import Linear.V2 (V2(..))
import Linear.Matrix (M22, det22, (!*))
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
type Machine = (Goal, ButtonA, ButtonB)
type Goal = V2 Int
type ButtonA = Button
type ButtonB = Button
type Button = V2 Int

solveEasy, solveHard :: Input -> Int
solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: Input -> Parsed
parseEasy = parseMachines

getResultEasy :: Parsed -> Int
getResultEasy = sum . mapMaybe getMachineScore

parseHard :: Input -> Parsed
parseHard = parseEasy

getResultHard :: Parsed -> Int
getResultHard = getResultEasy . map toHard
  where
    toHard (g, bA, bB) = ((+10000000000000) <$> g, bA, bB)

getMachineScore :: Machine -> Maybe Int
getMachineScore (g, V2 x1 y1, V2 x2 y2) = do
  let m = V2 (V2 x1 x2) (V2 y1 y2)
  let determinant = det22 m
  guard $ determinant /= 0
  let u = inverse22 m
  let ug = u !* g
  guard $ all ((== 0) . (`mod` determinant)) ug
  let V2 a b = (`div` determinant) <$> ug
  return $ 3 * a + b

inverse22 :: M22 Int -> M22 Int
inverse22 (V2 (V2 a b) (V2 c d)) = V2 (V2 d (-b)) (V2 (-c) a)

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