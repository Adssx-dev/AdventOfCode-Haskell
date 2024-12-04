module Solutions.Y2020.Day10
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List


part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ product differences
    where
        ln = lines inputStr
        intialNumberList = nub $ sort $ map read ln :: [Int]
        numberList = 0 : intialNumberList ++  [maximum intialNumberList + 3]
        differences = map length $ group $ sort $ zipWith (\ a b -> b - a) (init numberList) (tail numberList)

-- Only the number of successives ones matter for each group of ones
-- Number of possible path depending on the number of successive ones :
-- 1 -> 1
-- 2 -> 2
-- 3 -> 4
-- 4 -> 7
-- More -> IDK
part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ product products
    where
        ln = lines inputStr
        intialNumberList = nub $ sort $ map read ln :: [Int]
        numberList = 0 : intialNumberList ++  [maximum intialNumberList + 3]
        differences = map length $ filter (\x -> head x == 1) $ group $  zipWith (\ a b -> b - a) (init numberList) (tail numberList)
        products = map (\x -> [1, 2, 4, 7] !! (x-1)) differences

