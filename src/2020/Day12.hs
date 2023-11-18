import System.IO
import Debug.Trace
import Data.List
import Data.Maybe

data Direction = North | West | South | East deriving (Show, Eq)
data Coordinates = Coordinates {east :: Int, north :: Int} deriving (Show, Eq, Ord)

main = do
    handle <- openFile "data/2020/Day12.txt" ReadMode
    contents <- hGetContents handle


    let (finalCoordinates, _) =  navigate (Coordinates 0 0, East) $ lines contents
    print $ finalCoordinates 
    print $ abs(east finalCoordinates) + abs (north finalCoordinates)


    hClose handle

navigate :: (Coordinates, Direction) -> [[Char]] -> (Coordinates, Direction)
navigate coordinates [] = coordinates
navigate (coordinates, direction) (command:next) =  navigate (applyCommand coordinates command direction) next

applyCommand :: Coordinates -> [Char] -> Direction -> (Coordinates, Direction)
applyCommand initialCoordinates (command:parameter) currentDirection = case command of 
    'N' -> (move initialCoordinates North parameterInt, currentDirection)
    'W' -> (move initialCoordinates West parameterInt,  currentDirection)
    'S' -> (move initialCoordinates South parameterInt, currentDirection)
    'E' -> (move initialCoordinates East parameterInt,  currentDirection)
    'F' -> (move initialCoordinates currentDirection parameterInt, currentDirection)
    rot -> (initialCoordinates, rotate currentDirection rot parameterInt )
    where 
        parameterInt = read parameter :: Int

move Coordinates{east, north} direction number = case direction of
    North -> Coordinates east (north + number) 
    West  -> Coordinates (east - number) north
    South -> Coordinates east (north - number)
    East  -> Coordinates (east + number) north


rotate previousDirection _ 0 = previousDirection
rotate previousDirection 'L' 90 
    | previousDirection == North = West
    | previousDirection == West  = South
    | previousDirection == South = East
    | previousDirection == East  = North
rotate previousDirection 'R' 90 
    | previousDirection == North = East
    | previousDirection == West  = North
    | previousDirection == South = West
    | previousDirection == East  = South
rotate previousDirection dir angle = rotate (rotate previousDirection dir (angle - 90)) dir  90