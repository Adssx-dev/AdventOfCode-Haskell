module Utils.Map2D
where

import qualified Data.Map as Map
import Utils.Geometry
import Data.Maybe

parseInputToMap2d parser inputStr = foldl Map.union Map.empty linesParsed
    where
        lns = lines inputStr
        linesParsed = zipWith (\line idx -> parseLineMap2d  parser idx 0 line) lns [0..]


parseLineMap2d  :: (Char -> a) -> Int -> Int -> String -> Map.Map Point a
parseLineMap2d  parser lineNumber accum [] = Map.empty
parseLineMap2d  parser lineNumber accum (char:str) = Map.insert Point{x=accum, y=lineNumber} (parser char) (parseLineMap2d  parser lineNumber (accum + 1) str)

getNeighbors4Map2d map2d target = mapMaybe (`Map.lookup` map2d) neighborCoordinates
    where
        neighborCoordinates = getNeighbors4 target

getNeighbors8Map2d map2d target = mapMaybe (`Map.lookup` map2d) neighborCoordinates
    where
        neighborCoordinates = getNeighbors8 target