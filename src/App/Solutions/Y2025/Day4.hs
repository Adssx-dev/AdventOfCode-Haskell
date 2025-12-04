module Solutions.Y2025.Day4
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import qualified Data.Map as Map
import Utils.Map2D
import Utils.Geometry

data Cell = Empty | Roll deriving (Eq, Show, Ord)

charToCell '.' = Empty
charToCell '@' = Roll
charToCell chr = error $ "Could not parse character " ++ [chr]


isAccessible neighbors =  4 > length (filter (== Roll) neighbors)

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ length accessibleRolls
    where
        map2d = parseInputToMap2d charToCell inputStr
        allRollCoordinates = Map.keys $ Map.filter ( == Roll) map2d
        accessibleRolls = filter (isAccessible . getNeighbors8Map2d map2d) allRollCoordinates --map (getNeighbors4Map2d map2d) allRollCoordinates



part2 :: [Char] -> Maybe Int
--part2 inputStr = Just $ snd $ until (\res -> snd res == 0) removeRolls (map2d, 1)
part2 inputStr = Just totalRemoved
    where
        map2d = parseInputToMap2d charToCell inputStr
        (finalMap, lastRemove, totalRemoved) = until (\res -> extractSecond res == 0) removeRolls (map2d, 1, 0)

extractSecond :: (a, b, c) -> b
extractSecond (_, a, _) = a

extractThird :: (a, b, c) -> c
extractThird (_, _, a) = a

removeRolls :: (Map.Map Point Cell, Int, Int)-> (Map.Map Point Cell, Int, Int)
removeRolls (map2d, _, accum) = (foldl (\m p -> Map.insert p Empty m) map2d accessibleRolls, removedCount, removedCount + accum )
    where
        allRollCoordinates = Map.keys $ Map.filter ( == Roll) map2d
        accessibleRolls = filter (isAccessible . getNeighbors8Map2d map2d) allRollCoordinates --map (getNeighbors4Map2d map2d) allRollCoordinates
        removedCount = length accessibleRolls

