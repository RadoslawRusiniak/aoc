import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.Graph
import Data.List as L
import Data.List.Extra (splitOn)
import Data.Char
import Data.Foldable
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Text.Regex.Applicative (RE, string, sym, (<|>))
import Text.Regex.Applicative.Common (decimal)

main = do
    file <- getFileContents
    print . solveEasy $ file
    print . solveHard $ file
    where getFileContents = readFile. head =<< getArgs

type Rule = (Int, Int)
type Page = [Int]
type Input = (RuleBook, [Page])
type RuleBook = M.Map Int (S.Set Int)

solveEasy = getResultEasy. parseEasy
solveHard = getResultHard. parseHard

parseEasy :: String -> Input
parseEasy = (\[r,o] -> (parseRuleBook r, parseOrders o)). splitOn ["\r"]. lines
    where
    parseRuleBook :: [String] -> RuleBook
    parseRuleBook = M.fromListWith (<>). map (\(a, b) -> (a, S.singleton b)). parseRules
    parseRules :: [String] -> [Rule]
    parseRules = map parseRule
    parseRule = (\[a, b] -> (read a, read b)). splitOn "|"
    parseOrders :: [String] -> [Page]
    parseOrders = map parseOrder
    parseOrder = map read. splitOn ","

getResultEasy :: Input -> Int
getResultEasy (rules, pages) = sum. map getScore. filter (isOk rules) $ pages

getScore :: Page -> Int
getScore xs = xs !! (length xs `div` 2)

isOk :: RuleBook -> Page -> Bool
isOk _ [] = True
isOk rules (x:xs) = all (isOkOrdering rules. (x,)) xs && isOk rules xs
    where
    isOkOrdering :: RuleBook -> Rule -> Bool
    isOkOrdering rules (f, s) = case M.lookup s rules of
        Nothing -> True
        Just st -> not $ S.member f st

parseHard :: String -> Input
parseHard = parseEasy

getResultHard :: Input -> Int
getResultHard (r, p) = sum. map getScore. map (reorder r). filter (not. isOk r) $ p
    
reorder :: RuleBook -> Page -> Page
reorder r p = sorted
    where
    sorted = map ((\(k, _, _) -> k). nodeFromVertex). topSort $ graph
    (graph, nodeFromVertex, _) = graphFromEdges. getEdges $ r
    getEdges = map (\(k, v) -> (k, k, v)). M.assocs. M.map S.toList. getNodesOnPage
    getNodesOnPage = M.filterWithKey (\k _ -> S.member k nodesSet)
    nodesSet = S.fromList p
