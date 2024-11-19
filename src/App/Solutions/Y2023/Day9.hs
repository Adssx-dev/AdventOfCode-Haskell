module Solutions.Y2023.Day9
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char

import qualified Data.Map as Map

part1 inputStr = Just $ sum nextElements
    where
        lists = map (\x -> map read (words x) :: [Int]) (lines  inputStr)
        nextElements = map predictNext lists

part2 inputStr = Just $ sum nextElements
    where
        lists = map (\x -> map read (words x) :: [Int]) (lines  inputStr)
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