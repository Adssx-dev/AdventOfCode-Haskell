module Solutions.Y2024.Day2
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List


part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ length $ filter validateReport $ parseReport inputStr


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ length $ filter (any validateReport . genListOneElementMissing) $ parseReport inputStr 

parseReport inputStr = map (map read . words) $ lines inputStr :: [[Int]]

validateReport report = (areAllIncreasing || areAllDecreasing) && areAtMost3 && areAtLeast1
    where
        diff = zipWith (-) (tail report) report
        areAllIncreasing = all (>0) diff
        areAllDecreasing = all (<0) diff
        areAtMost3 = all ((<= 3) . abs) diff
        areAtLeast1 = all ((>= 1) . abs) diff

genListOneElementMissing [x] = [[x], []]
genListOneElementMissing (x:xs) = map (x:) recurse ++ [head recurse]
    where 
        recurse = genListOneElementMissing xs