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

    let lists = map (\x -> map read (words x) :: [Int]) (lines  contents)

    print $ part1 lists
    print $ part2 lists

    hClose handle

part1 lists = sum nextElements
    where
        nextElements = map predictNext lists

part2 lists = sum nextElements
    where
        nextElements = map predictPrevious lists

predictNext :: [Int] -> Int
predictNext lst = if all (==0) derivative 
    then last lst
    else last lst + predictNext derivative
    where
        derivative = derivate lst

predictPrevious :: [Int] -> Int
predictPrevious lst = if all (==0) derivative 
    then head lst
    else head lst - predictPrevious derivative
    where
        derivative = derivate lst


derivate lst = zipWith (-) (tail lst) lst