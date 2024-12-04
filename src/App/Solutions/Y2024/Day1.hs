module Solutions.Y2024.Day1
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List


part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum $ map (\(x:y:xs) -> abs $ y - x ) $ transpose $ map sort lists
    where
        lists = parseLists inputStr


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ sum  $ zipWith (*) listL (map (\x -> length $ filter (==x) listR) listL)
    where
        [listL, listR] = parseLists inputStr

parseLists inputStr = map (map read) $ transpose $ map words (lines inputStr) :: [[Int]]