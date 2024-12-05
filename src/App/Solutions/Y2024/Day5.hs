{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Solutions.Y2024.Day5
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import qualified Data.Set as Set
import Utils.SplitOn

type RuleSet = Set.Set (Int, Int)

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum $ map middleValue $ filter (isOrderOk rules) updates
    where
        (rules, updates) = parseRules (lines inputStr) Set.empty []


-- Generate all pairs of coordinates up to a given maximum without duplicate
-- Ex : Length = 5 => [(1,2),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,4),(3,5),(4,5)]
generateAllPairs length = concatMap (\x -> map (x,) [(x+1)..max]) [0..max]
    where
        max = length - 1

isOrderOk :: (Ord a, Show a) => Set.Set (a, a) -> [a] -> Bool
isOrderOk ruleSet list = all (\(id1, id2) -> isRuleOk ruleSet (list !! id1) (list !! id2)) $ generateAllPairs $ length list

-- Checks whether the two values passed are in the right order accorging to rules
isRuleOk ruleSet first second = case (Set.member (first, second) ruleSet, Set.member (second, first) ruleSet) of
        (False, False) -> True -- Rules state nothing => ok
        (True, False) -> True  -- Rules states that first should be before second => ok
        (False, True) -> False -- Rules states that second should be before first => not ok
        (True, True) -> error ("Something is terribly wrong : Cycle detected in rules between " ++ show first ++ " and " ++ show second )

parseRules :: [[Char]] -> RuleSet -> [[Int]] -> (RuleSet, [[Int]])
parseRules [] rules updates = (rules, updates)
parseRules (current:remaining) rules updates = case length current of
        0 -> parseRules remaining rules updates
        5 -> parseRules remaining (Set.insert newRule rules) updates
        _ -> parseRules remaining rules (newUpdate:updates)
    where 
        newRule = tuplify2 $ (map read $ splitOn (=='|') current :: [Int])
        newUpdate = map read $ splitOn (==',') current :: [Int]

tuplify2 [x,y] = (x,y)

middleValue list = list !! div (length list)  2



part2 :: [Char] -> Maybe Int
part2 inputStr = Nothing
