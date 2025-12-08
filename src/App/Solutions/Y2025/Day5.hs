{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Solutions.Y2025.Day5
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.List
import Utils.Geometry2D

data Range = Range Int Int
type Id = Int

parseInput :: [Char] -> ([Range], [Id])
parseInput inputStr = (rangesParsed, idsParsed)
    where
        lns = lines inputStr
        [ranges, ids] = splitOn (== "") lns
        rangesParsed = map ((\[a, b] -> Range (read a) (read b)) . splitOn (=='-')) ranges :: [Range]
        idsParsed = map read ids :: [Id]

isIdValid :: Foldable t => t Range -> Int -> Bool
isIdValid ranges id = any (\(Range a b) -> id >= a && id <= b) ranges

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ length $ filter (isIdValid ranges) ids
    where
        (ranges, ids) = parseInput inputStr


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ sum $ map rangeSize finalRanges
    where
        (ranges, ids) = parseInput inputStr
        finalRanges = foldl smartInsert [] ranges

-- Insert the range and if there is a collision, merge the ranges 
smartInsert :: [Range] -> Range -> [Range]
smartInsert [] newRange = [newRange]
smartInsert ((Range a1 a2):initialRanges) (Range b1 b2) = if isCollision
    then smartInsert initialRanges (Range (min a1 b1) (max a2 b2))
    else Range a1 a2:smartInsert initialRanges (Range b1 b2)
    where
        isCollision = collision1d a1 a2 b1 b2

rangeSize (Range a b) = b - a + 1