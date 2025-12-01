module Solutions.Y2025.Day1
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List


part1 :: [Char] -> Maybe Int
part1 inputStr = Just numberOfZeros
    where
        entries = lines inputStr
        functions = map lineToFunctionPt1 entries
        values = scanl (\x f -> f x) 50 functions
        numberOfZeros = length $ filter (== 0) values

lineToFunctionPt1 ('R':val) x = mod (x + (read val :: Int)) 100
lineToFunctionPt1 ('L':val) x = mod (x - (read val :: Int)) 100


part2 :: [Char] -> Maybe Int
part2 inputStr = Just numberOfZeros
    where
        entries = lines inputStr
        functions = concatMap lineToFunctionPt2 entries
        values = scanl (\x f -> f x) 50 functions
        numberOfZeros = length $ filter (== 0) values


lineToFunctionPt2 :: [Char] -> [Int -> Int]
lineToFunctionPt2 ('R':val) = replicate (read val :: Int) (\x -> mod (x + 1) 100)
lineToFunctionPt2 ('L':val) = replicate (read val :: Int) (\x -> mod (x - 1) 100)