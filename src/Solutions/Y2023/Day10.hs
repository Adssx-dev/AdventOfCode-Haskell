import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char

import qualified Data.Map as Map

data Coordinates = Coordinates {row :: Int, col :: Int} deriving (Show, Eq, Ord)

main = do
    handle <- openFile "data/2023/Day10.txt" ReadMode
    contents <- hGetContents handle

    let map2d = lines contents
    let startingPoint = startingPointCoordinates map2d 0
    print $ startingPoint
    let sCoords = getPointIn2DMap map2d Coordinates{row=2,col=3}

    let a = length $ fromMaybe [] $ browseTunnel True startingPoint Coordinates{row=row startingPoint - 1,col= col startingPoint} [startingPoint] map2d
    let b = length $ fromMaybe [] $ browseTunnel True startingPoint Coordinates{row=row startingPoint + 1,col= col startingPoint} [startingPoint] map2d
    let c = length $ fromMaybe [] $ browseTunnel True startingPoint Coordinates{row=row startingPoint,col= col startingPoint + 1} [startingPoint] map2d
    let d = length $ fromMaybe [] $ browseTunnel True startingPoint Coordinates{row=row startingPoint,col= col startingPoint - 1} [startingPoint] map2d
    let path = maximum [a, b, c, d]

    if even path
    then print $ fromIntegral path / 2
    else print $  fromIntegral(path -1) / 2


    --print $ browseTunnel True Coordinates{row=1,col=1} Coordinates{row=1,col=2} [Coordinates{row=1,col=1}] map2d

    --print $ part1 lists
    --print $ part2 lists

    hClose handle

--browseTunnel Coordinates{row=row, col=col} Coordinates{row=row, col=col} pastCoordinates _  = pastCoordinates
browseTunnel first targetCoordinates currentCoordinates pastCoordinates map2d
    | targetCoordinates == currentCoordinates = Just pastCoordinates
    | isNothing currentPipe = Nothing
    | not $ null nextCoordinates = browseTunnel False targetCoordinates (head nextCoordinates) (currentCoordinates:pastCoordinates) map2d
    | otherwise = Nothing
    where
        currentPipe = getPointIn2DMap map2d currentCoordinates
        nextCoordinates = filter (\x -> x `notElem` pastCoordinates || (not first && x == targetCoordinates)) $ neighborCoordinates (row currentCoordinates) (col currentCoordinates) (fromMaybe ' ' currentPipe)

getPointIn2DMap :: [[Char]] -> Coordinates -> Maybe Char
getPointIn2DMap map2d coords = if or [row coords < 0, row coords > mapHeight, col coords < 0, col coords > mapWidth]
    then Nothing
    else Just ((map2d !! row coords) !! col coords)
    where
        mapHeight = length map2d
        mapWidth = length $ head map2d

startingPointCoordinates :: [[Char]] -> Int -> Coordinates
startingPointCoordinates (dataRow:mapRemaining) curRow = case elemIndex 'S' dataRow of
    Just val -> Coordinates{row=curRow, col=val}
    Nothing -> startingPointCoordinates mapRemaining (curRow + 1)




-- loadLine [] _ _ tree = tree
-- loadLine (symb:remaining) row (col:nextCols) tree = loadLine remaining row nextCols newTree
--     where
--         currentCoordinates = Coordinates {row, col}
--         connections = case symb of




neighborCoordinates row col '|' = [Coordinates {row=row-1, col=col}, Coordinates {row=row+1, col}]
neighborCoordinates row col '-' = [Coordinates {row=row, col=col-1}, Coordinates {row, col=col+1}]
neighborCoordinates row col 'L' = [Coordinates {row=row-1, col=col}, Coordinates {row, col=col+1}]
neighborCoordinates row col 'J' = [Coordinates {row=row-1, col=col}, Coordinates {row, col=col-1}]
neighborCoordinates row col 'F' = [Coordinates {row=row+1, col=col}, Coordinates {row, col=col+1}]
neighborCoordinates row col '7' = [Coordinates {row=row+1, col=col}, Coordinates {row, col=col-1}]
neighborCoordinates row col '.' = []
neighborCoordinates row col char = error ("character unrecognised :" ++ [char])

