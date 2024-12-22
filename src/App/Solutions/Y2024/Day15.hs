{-# LANGUAGE RecordWildCards #-}
module Solutions.Y2024.Day15
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import qualified Data.Map as Map
import Utils.List
import Utils.Matrix 
import Utils.Geometry

data Cell = Crate | Wall | Empty | Character
    deriving(Ord, Show, Eq)

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum $ map (gpsCoordinate . fst) $ filter ((== Crate) . snd) $ Map.toList $ fst finalWarehouse
    where
        (warehouse, directions, robotPos) = parseInput inputStr
        finalWarehouse = simulate warehouse directions robotPos
        

cellFromAscii '#' = Wall
cellFromAscii 'O' = Crate
cellFromAscii '.' = Empty
cellFromAscii '@' = Character

simulate warehouse directions robotPos = foldl step (warehouse, robotPos) directions

gpsCoordinate Point{..} = y * 100 + x

parseInput inputStr = (warehouse, directions, robotPos)
    where
        [warehouseStr, movements] = splitOn null $ lines inputStr
        warehouseWithPlayer = toCoordinateList warehouseStr
        robotPos = fst $ head $ filter (('@' ==) . snd ) warehouseWithPlayer
        warehouse = Map.map cellFromAscii $ Map.insert robotPos '.' $ Map.fromList warehouseWithPlayer
        directions = concatMap (map vectorFromAscii) movements


step :: (Map.Map Point Cell, Point) -> Vector -> (Map.Map Point Cell, Point)
step (warehouse, robotPosition) direction = case Map.lookup newPos newWarehouse of
    Just Empty -> (newWarehouse, newPos)
    _ -> (newWarehouse, robotPosition)
    where
        newPos = apply direction robotPosition
        warehouseChanges = cratesToMove warehouse newPos direction
        newWarehouse = foldl (\w (p, s) -> Map.insert p s w) warehouse warehouseChanges

-- Position = where the robot tries to go
cratesToMove :: Map.Map Point Cell -> Point -> Vector -> [(Point, Cell)]
cratesToMove warehouse position direction = cratesToMoveInternal warehouse position direction [(position, Empty)]

cratesToMoveInternal :: Map.Map Point Cell -> Point -> Vector -> [(Point, Cell)] -> [(Point, Cell)]
cratesToMoveInternal warehouse position direction actionStack = case currentCell of
    Just Empty -> actionStack
    Just Wall -> [] -- Impossible to move
    Just Crate -> cratesToMoveInternal warehouse nextPos direction (newAction:actionStack)
    Nothing -> error $ "Out of bounds : " ++ show position
    where
        nextPos = apply direction position
        currentCell = Map.lookup position warehouse 
        newAction = (nextPos, Crate)

part2 :: [Char] -> Maybe Int
part2 inputStr = Nothing
