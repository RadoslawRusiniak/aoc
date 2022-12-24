import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List.Split
import Data.List
import Data.Char (ord, isLower)

main = do  
    contents <- getFileContents
    print. solve $ contents

getFileContents :: IO String
getFileContents = readFile . head =<< getArgs


solve :: String -> Int
solve = sum. map (getScore. getLetter). toTriples. lines
    where
    toTriples :: [String] -> [(String, String, String)]
    toTriples = map (\lst -> (lst !! 0, lst !! 1, lst !! 2)). chunksOf 3
    getLetter :: (String, String, String) -> Char
    getLetter (f, s, t) = head. filter (\e -> elem e s && elem e t) $ f 

    getScore :: Char -> Int
    getScore l = if isLower l then ord(l) - ord('a') + 1 else ord(l) - ord('A') + 27
