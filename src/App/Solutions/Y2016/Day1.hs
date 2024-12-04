module Solutions.Y2016.Day1
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Data.Char
import Data.Set

data Coordinates = Coordinates {row :: Int, col :: Int} deriving (Show, Eq, Ord)

part1 inputStr = Just $ abs(row position) + abs(col position)
    where
        position = executeAll (words inputStr) Coordinates{row=0,col=0} Coordinates{row=0, col=1}

part2 inputStr = Just $ abs(row position) + abs(col position)
    where
        position = executeAllFindVisited (words inputStr) Coordinates{row=0,col=0} Coordinates{row=0, col=1} $ Data.Set.fromList [Coordinates{row=0,col=0}]


executeAll [] position _ = position
executeAll (c:commands) position direction =  executeAll commands newPosition newDirection
    where
        (newPosition, newDirection) = executeCommand c position direction

executeAllFindVisited :: [[Char]] -> Coordinates -> Coordinates -> Set Coordinates -> Coordinates
executeAllFindVisited [] _ _ _ = error "Reached end without visiting spot twice"
executeAllFindVisited (c:commands) position direction visited = if Data.Set.member newPosition visited
            then newPosition
            else trace (show (newPosition, visited))  executeAllFindVisited commands newPosition newDirection allVisited
    where
        (newPosition, newDirection) = executeCommand c position direction
        allVisited = Data.Set.insert newPosition visited


executeCommand (dirMod:length) coords currentDirection = (iterate (addCoordinates newDirection) coords !! n, newDirection)
    where
        n = read (Data.List.filter isDigit length) :: Int -- parse count with maybe trailing coma
        newDirection = if dirMod == 'L' then turnLeft currentDirection else turnRight currentDirection

-- Coordinates are used as vectors in 2-D space
turnLeft :: Coordinates -> Coordinates
turnLeft Coordinates{row=r, col=c} = Coordinates{row= -c, col=r}

turnRight :: Coordinates -> Coordinates
turnRight Coordinates{row=r, col=c} = Coordinates{row= c, col= -r}

addCoordinates :: Coordinates -> Coordinates -> Coordinates
addCoordinates Coordinates{row=row1, col=col1} Coordinates{row=row2, col=col2} = Coordinates{row = row1 + row2, col = col1 + col2}

