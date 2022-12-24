import System.IO  
import Control.Monad
import Data.List.Split
import Data.List

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let ans = solve. lines $ contents
        print ans
        hClose handle  

solve :: [String] -> Int
solve = sum .take 3 .reverse .sort .total
    where
        total = map (sum .convert) .group
        group = splitOn [""]
        convert = map read
