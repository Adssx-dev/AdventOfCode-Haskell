module Solutions.Y2024.Day12
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.Matrix
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils.Geometry
import Utils.List
import Data.Maybe (listToMaybe)

data Area = Area
    { symbol :: Char
    , points :: Set.Set Point
    } deriving (Eq, Show, Ord)


part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ test garden
    where
        garden = toCoordinateList $ lines inputStr

-- trace (show $ map (\x -> (x, perimeter x, area x)) allGroups) 
test :: [(Point, Char)] -> Int
test list = sum $ map (\x -> perimeter x * area x) allGroups
    where
        allGroups = concatMap (\x -> merge (snd x) []) x
        x = map (\el -> (snd $ head el, Set.fromList $ map fst el)) $
            groupBy (\x y -> snd x == snd y) $
            sortBy (\a b -> compare (snd a) (snd b)) $ list

test2 :: [(Point, Char)] -> Int
test2 list = trace (show (map (\x -> (perimeterPt2 x, area x)) allGroups)) sum $ map (\x -> perimeterPt2 x * area x) allGroups
    where
        allGroups = concatMap (\x -> merge (snd x) []) x
        x = map (\el -> (snd $ head el, Set.fromList $ map fst el)) $
            groupBy (\x y -> snd x == snd y) $
            sortBy (\a b -> compare (snd a) (snd b)) $ list

merge :: Set.Set Point -> [Set.Set Point] -> [Set.Set Point]
merge pool res = if Set.null pool
    then res
    else merge poolAfterMerge (mergedGroup:res)
    where
        firstElem = headOfSet pool
        newPool = Set.delete firstElem pool
        (poolAfterMerge, mergedGroup) = mergeOne firstElem (pool, Set.empty)

area :: Set.Set a -> Int
area = Set.size

perimeter :: Set.Set Point -> Int
perimeter set = length x
    where
        asList = Set.toList set
        x =  concatMap (filter (`notElem` asList) . getNeighbors4) asList

perimeterPt2 :: Set.Set Point -> Int
perimeterPt2 set = length grouped
    where
        asList = Set.toList set
        x = Set.fromList $ concatMap (filter (`notElem` asList) . getNeighbors4) asList
        grouped = merge x []

headOfSet = head . Set.toList

mergeOne ::  Point -> (Set.Set Point, Set.Set Point) -> (Set.Set Point, Set.Set Point)
mergeOne point (pool, res) = if Set.member point pool
        then (p4, Set.insert point r4)
        else (pool, res)
    where
        neighbors = getNeighbors4 point
        newPool = Set.delete point pool
        (p1, r1) = mergeOne (head neighbors) (newPool, res)
        (p2, r2) = mergeOne (neighbors !! 1) (p1, r1)
        (p3, r3) = mergeOne (neighbors !! 2) (p2, r2)
        (p4, r4) = mergeOne (neighbors !! 3) (p3, r3)


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ test2 garden
    where
        garden = toCoordinateList $ lines inputStr
