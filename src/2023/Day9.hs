import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char

import qualified Data.Map as Map

main = do
    handle <- openFile "data/2023/Day9.txt" ReadMode
    contents <- hGetContents handle

    print $ predictNext [10, 13, 16, 21, 30, 45]
    print $ part1 (lines contents)
    --print $ part2 (lines contents)

    hClose handle

part1 lines = sum nextElements
    where
        lists = map (\x -> map read (words x) :: [Int]) lines 
        nextElements = map predictNext lists

predictNext :: [Int] -> Int
predictNext lst = if all (==0) derivative 
    then last lst
    else last lst + predictNext derivative
    where
        derivative = derivate lst

derivate lst = zipWith (-) (tail lst) lst