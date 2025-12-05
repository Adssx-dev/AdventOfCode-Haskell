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
part2 inputStr = Nothing
