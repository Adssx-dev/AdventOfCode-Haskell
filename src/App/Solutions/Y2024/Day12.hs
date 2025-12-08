{-# LANGUAGE RecordWildCards #-}
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
import Utils.Geometry2D
import Utils.List
import Data.Maybe (listToMaybe)

data Area = Area
    { symbol :: Char
    , points :: Set.Set Point
    } deriving (Eq, Show, Ord)


part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ solvePt1 garden
    where
        garden = toCoordinateList $ lines inputStr

solvePt1 :: [(Point, Char)] -> Int
solvePt1 list = sum $ map (\x -> perimeter x * area x) allGroups
    where
        allGroups = concatMap (\x -> merge (snd x) []) x
        x = map (\el -> (snd $ head el, Set.fromList $ map fst el)) $
            groupBy (\x y -> snd x == snd y) $
            sortBy (\a b -> compare (snd a) (snd b)) $ list

solvePt2 :: [(Point, Char)] -> Int
solvePt2 list = sum $ map (\x -> perimeterPt2 x * area x) allGroups
    where
        allGroups = concatMap (\x -> merge (snd x) []) x
        x = map (\el -> (snd $ head el, Set.fromList $ map fst el)) $
            groupBy (\x y -> snd x == snd y) $
            sortBy (\a b -> compare (snd a) (snd b)) $ list

merge :: Set.Set Point ->
        [Set.Set Point] ->
        [Set.Set Point]
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
perimeterPt2 set = sum $ map (countCorners set) (Set.toList set)

isCorner :: Set.Set Point -> (Point, Point, Point) -> Bool
isCorner area (right1, diag, right2) = 
        not (right1InSet || right2InSet) ||
        (right1InSet && right2InSet && not diagInSet)
    where
        right1InSet = Set.member right1 area
        diagInSet = Set.member diag area
        right2InSet = Set.member right2 area

countCorners :: Set.Set Point -> Point -> Int
countCorners area point = length $ filter id $ map (isCorner area) corners
    where
        corners = generateCorners point

generateCorners :: Point -> [(Point, Point, Point)]
generateCorners Point{..} = [
        (Point{x=x-1, y=y}, Point{x=x-1, y=y-1}, Point{x=x, y=y-1}),
        (Point{x=x, y=y-1}, Point{x=x+1, y=y-1}, Point{x=x+1, y=y}),
        (Point{x=x+1, y=y}, Point{x=x+1, y=y+1}, Point{x=x, y=y+1}),
        (Point{x=x, y=y+1}, Point{x=x-1, y=y+1}, Point{x=x-1, y=y})
    ]


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
part2 inputStr = Just $ solvePt2 garden
    where
        garden = toCoordinateList $ lines inputStr
