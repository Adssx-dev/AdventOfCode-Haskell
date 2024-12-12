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
part1 inputStr = Nothing
    where
        garden = toCoordinateMap $ lines inputStr


test :: [(Point, Char)] -> Int
test list = 0
    where
        x = map (\el -> (snd $ head el, map fst el)) $
            groupBy (\x y -> fst x == fst y) $
            sortBy (\a b -> compare (fst a) (fst b)) $ list


merge :: Set.Set Point -> Set.Set Point -> [Set.Set Point]
merge pool constructed = []
    where
        firstElem = head . Set.toList $ pool
        newPool1 = Set.delete firstElem pool
        neighbors = Set.fromList $  getNeighbors4 firstElem
        foundNeighbors = Set.union newPool1 neighbors
        newPool2 = Set.difference newPool1 foundNeighbors


-- findArea :: Map.Map Point Char -> [Area] -> Set.Set Point -> [Area]
-- findArea garden allAreas candidates = if Set.null candidates
--         then allAreas
--         else findArea garden (newArea:allAreas) (Set.difference candidates allUsedPoints)
--     where
--         (newArea, newCandidates) = findPointsInArea garden
--         currentCand = map (`Map.lookup` garden) candidates
--         allUsedPoints




-- findPointsInArea :: Map.Map Point Char -> Area -> Set.Set Point -> Point -> (Area, Set.Set Point)
-- findPointsInArea garden currentArea futureCandidates pos = if currentPointValue == symbol currentArea
--         then 
--         else
--     where
--         currentPointValue = Map.lookup pos garden
--         neighbors = getNeighbors4 pos


part2 :: [Char] -> Maybe Int
part2 inputStr = Nothing
