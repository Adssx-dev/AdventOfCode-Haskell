{-# LANGUAGE RecordWildCards #-}
module Solutions.Y2024.Day6
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.Matrix
import qualified Data.Map as Map
import Utils.Geometry
import qualified Data.Set as Set
import qualified Control.Applicative as Set.Set
import Data.Either

data Cell = Empty | Blocked | Guard deriving (Eq, Show)

-- Simply count the positions travelled
part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ Set.size $ travelledLocations inputStr
        
-- List all locations travelled by the guard for a given map
travelledLocations inputStr = travel
    where   
        grid = toCoordinateMap $ map (map charToCell) $ lines inputStr
        gridAsList = Map.toList grid
        guardInitialPosition = fst $ head $ filter (\x -> Guard == snd x) gridAsList
        guardInitialDirection = Vector {dx = 0, dy = -1}
        Right travel = until isRight (nextPosition grid) (Left (guardInitialPosition, guardInitialDirection, Set.fromList [guardInitialPosition]))

-- Calculate the next position of the guard from the current position, the direction and the map
-- When finished, returns "Right" with the set of all coordinates traversed
nextPosition :: Map.Map Point Cell -> Either (Point, Vector, Set.Set Point)  (Set.Set Point)-> Either (Point, Vector, Set.Set Point) (Set.Set Point)
nextPosition grid (Left (pos, dir, previousPos)) = case nextCell of
        Nothing -> Right previousPos
        Just Empty -> Left (forwardPos, dir, Set.insert forwardPos previousPos)
        Just Guard -> Left (forwardPos, dir, Set.insert forwardPos previousPos)
        Just Blocked -> Left (pos, dirRotated, Set.insert rightPos previousPos)
    where
        forwardPos = apply dir pos
        nextCell = Map.lookup forwardPos grid
        dirRotated = turnRight dir
        rightPos = apply dirRotated pos

-- Basically the same as for part 1, but stores all positions + directions
-- A cycle is detected when we get back the the same tuple (position, direction)
-- When done, get back a "Right" value with a boolean:
-- True if a cycle is detected
-- False if the guard went out of the map
nextPositionPt2 :: Map.Map Point Cell -> Either (Point, Vector, Set.Set (Point, Vector))  Bool-> Either (Point, Vector, Set.Set (Point, Vector)) Bool
nextPositionPt2 grid (Left (pos, dir, previousPos)) =  case (Set.member (pos, dir) previousPos, nextCell) of
        (True, _) -> Right True
        (False, Nothing) -> Right False
        (False, Just Blocked) -> Left (pos, dirRotated, Set.insert (pos, dir) previousPos)
        (False, Just _) -> Left (forwardPos, dir, Set.insert (pos, dir) previousPos)
    where
        forwardPos = apply dir pos
        nextCell = Map.lookup forwardPos grid
        dirRotated = turnRight dir
        rightPos = apply dirRotated pos

part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ length $ filter id res
    where
        grid = toCoordinateMap $ map (map charToCell) $ lines inputStr
        gridAsList = Map.toList grid
        guardInitialPosition = fst $ head $ filter (\x -> Guard == snd x) gridAsList
        guardInitialDirection = Vector {dx = 0, dy = -1}
        possibleObstacles = Set.toList $ travelledLocations inputStr
        res = map (\x -> doesCaseLoop (Map.insert x Blocked grid) guardInitialPosition guardInitialDirection) possibleObstacles



doesCaseLoop :: Map.Map Point Cell -> Point -> Vector -> Bool
doesCaseLoop grid guardInitialPosition guardInitialDirection = res
    where
        Right res = until isRight (nextPositionPt2 grid) init
        init = Left (guardInitialPosition, guardInitialDirection, Set.fromList [])

charToCell '.' = Empty
charToCell '#' = Blocked
charToCell '^' = Guard
charToCell c = error $ "Unexpected cell type : " ++ [c]