import System.IO  
import System.Environment (getArgs)
import Control.Monad
import Data.List.Split
import Data.List

main = do  
    contents <- getFileContents
    print. solve $ contents

getFileContents :: IO String
getFileContents = readFile . head =<< getArgs


solve :: String -> Int
solve = sum. map (getScore. readLine). lines
    where
        getScore (u, v) = getWorth v + getResult u v

        getWorth "R" = 1
        getWorth "P" = 2
        getWorth "S" = 3

        getResult "R" "P" = 6
        getResult "P" "S" = 6
        getResult "S" "R" = 6
        getResult u v = if u == v then 3 else 0
    

readLine :: String -> (String, String)
readLine = (\(u, v) -> (u, convertMine u v))
            . (\arr -> (convertOpp $ arr !! 0, arr !! 1))
            . words
    where
        convertOpp "A" = "R"
        convertOpp "B" = "P"
        convertOpp "C" = "S"
        convertMine :: String -> String -> String
        convertMine o "Y" = o
        convertMine o "X" = fst. head. filter ((== o). snd) $ paired 
        convertMine o "Z" = snd. head. filter ((== o). fst) $ paired

        nxt = ["R", "P", "S", "R"]
        paired = zip nxt (tail nxt)

