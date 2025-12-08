module Solutions.Y2024.Day10
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.Matrix
import qualified Data.Map as Map
import Data.Char
import Utils.Geometry2D


part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum ends
    where 
        ends = map (length . nub) $ findReachableEnds inputStr

findReachableEnds inputStr = map (findPaths grid 0)  startingCandidates
    where
        grid = toCoordinateMap $ map (map digitToInt) $ lines inputStr
        startingCandidates = map fst $
            filter ((== 0) . snd) $
            Map.toList 
            grid

-- Given one point, give all the "9" that can be reached
-- The result can be exploited either by removing duplicates for each starting point (answer to part 1) ot just counting the total for each start (answer to part 2)
findPaths _ 9 point = [point]
findPaths trailMap value point = concatMap (findPaths trailMap $ value + 1) neighbors
    where
        neighbors = filter (\n -> Just (value + 1) == Map.lookup n trailMap) $ getNeighbors4 point

part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ sum ends
    where 
        ends = map length $ findReachableEnds inputStr
