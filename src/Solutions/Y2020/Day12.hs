import System.IO
import Debug.Trace
import Data.List
import Data.Maybe

data Direction = North | West | South | East deriving (Show, Eq)
data Coordinates = Coordinates {east :: Int, north :: Int} deriving (Show, Eq, Ord)

main = do
    handle <- openFile "data/2020/Day12.txt" ReadMode
    contents <- hGetContents handle


    let (finalCoordinates, _) =  navigatePt1 (Coordinates 0 0, East) $ lines contents
    print finalCoordinates
    print $ abs (east finalCoordinates) + abs (north finalCoordinates)


    let finalCoordinatesPt2 =  navigatePt2 (Coordinates 0 0, Coordinates 10 1) $ lines contents
    print finalCoordinatesPt2
    print $ abs (east finalCoordinatesPt2) + abs (north finalCoordinatesPt2)


    hClose handle

navigatePt2 :: (Coordinates, Coordinates) -> [[Char]] -> Coordinates
navigatePt2 (shipCoordinates, waypointCoordinates) [] = shipCoordinates
navigatePt2 (shipCoordinates, waypointCoordinates) (command:next) = navigatePt2 (applyCommandPt2 (shipCoordinates, waypointCoordinates) command) next


applyCommandPt2 (shipCoordinates, waypointCoordinates) (command:parameter) = case command of
    'N' -> (shipCoordinates, move waypointCoordinates North parameterInt)
    'W' -> (shipCoordinates, move waypointCoordinates West parameterInt)
    'S' -> (shipCoordinates, move waypointCoordinates South parameterInt)
    'E' -> (shipCoordinates, move waypointCoordinates East parameterInt)
    'F' -> (moveToWaypoint shipCoordinates waypointCoordinates parameterInt, waypointCoordinates)
    rot -> (shipCoordinates, rotatePt2 waypointCoordinates rot parameterInt )
    where
        parameterInt = read parameter :: Int

rotatePt2 previousCoordinates _ 0 = previousCoordinates
rotatePt2 previousCoordinates 'L' 90 = Coordinates ((-1) * north previousCoordinates) (east previousCoordinates) 
rotatePt2 previousCoordinates 'R' 90 = Coordinates (north previousCoordinates) ((-1) * east previousCoordinates) 
rotatePt2 previousDirection dir angle = rotatePt2 (rotatePt2 previousDirection dir (angle - 90)) dir  90



navigatePt1 :: (Coordinates, Direction) -> [[Char]] -> (Coordinates, Direction)
navigatePt1 coordinates [] = coordinates
navigatePt1 (coordinates, direction) (command:next) =  navigatePt1 (applyCommandPt1 coordinates command direction) next

applyCommandPt1 :: Coordinates -> [Char] -> Direction -> (Coordinates, Direction)
applyCommandPt1 initialCoordinates (command:parameter) currentDirection = case command of
    'N' -> (move initialCoordinates North parameterInt, currentDirection)
    'W' -> (move initialCoordinates West parameterInt,  currentDirection)
    'S' -> (move initialCoordinates South parameterInt, currentDirection)
    'E' -> (move initialCoordinates East parameterInt,  currentDirection)
    'F' -> (move initialCoordinates currentDirection parameterInt, currentDirection)
    rot -> (initialCoordinates, rotatePt1 currentDirection rot parameterInt )
    where
        parameterInt = read parameter :: Int

moveToWaypoint shipCoordinates waypointCoordinates amount = Coordinates 
    (east shipCoordinates + amount * east waypointCoordinates)  
    (north shipCoordinates + amount * north waypointCoordinates)


move Coordinates{east, north} direction number = case direction of
    North -> Coordinates east (north + number)
    West  -> Coordinates (east - number) north
    South -> Coordinates east (north - number)
    East  -> Coordinates (east + number) north


rotatePt1 previousDirection _ 0 = previousDirection
rotatePt1 previousDirection 'L' 90
    | previousDirection == North = West
    | previousDirection == West  = South
    | previousDirection == South = East
    | previousDirection == East  = North
rotatePt1 previousDirection 'R' 90
    | previousDirection == North = East
    | previousDirection == West  = North
    | previousDirection == South = West
    | previousDirection == East  = South
rotatePt1 previousDirection dir angle = rotatePt1 (rotatePt1 previousDirection dir (angle - 90)) dir  90