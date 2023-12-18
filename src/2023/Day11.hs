import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char

import qualified Data.Map as Map

data Coordinates = Coordinates {row :: Int, col :: Int} deriving (Show, Eq, Ord)

main = do
    handle <- openFile "data/2023/Day11.txt" ReadMode
    contents <- hGetContents handle

    print $ part1 $ lines contents
    print $ part2 $ lines contents

    hClose handle

part2 lines = galaxiesDistancesWithExpansion galaxies expandedRows expandedCols 999999
    where
        space = lines
        galaxies = galaxiesCoordinates space 0
        expandedRows = emptyRowsIdx space
        expandedCols = emptyColsIdx space


part1 lines = galaxiesDistances galaxies
    where
        space = transpose $ expandSpace lines
        galaxies = galaxiesCoordinates space 0


emptyRowsIdx :: [[Char]] -> [Int]
emptyRowsIdx space = elemIndices True $ map (all (=='.')) space

emptyColsIdx :: [[Char]] -> [Int]
emptyColsIdx space = emptyRowsIdx $ transpose space

galaxiesDistances [] = 0
galaxiesDistances (gal:galaxies) = galaxiesDistances galaxies + sum (map (calculateDistance gal) galaxies)

galaxiesDistancesWithExpansion [] _ _ _ = 0
galaxiesDistancesWithExpansion (gal:galaxies) expandedRowsIdx expandedColsIdx expansionFactor 
    =  galaxiesDistancesWithExpansion galaxies expandedRowsIdx expandedColsIdx expansionFactor + sum (map (calculateDistanceWithExpansion gal expandedRowsIdx expandedColsIdx  expansionFactor) galaxies)


calculateDistance :: Coordinates -> Coordinates -> Int
calculateDistance Coordinates{row=row1, col=col1} Coordinates{row=row2,col=col2} = abs (row2 - row1) + abs (col2 - col1)

calculateDistanceWithExpansion 
    Coordinates{row=row1, col=col1} 
    expandedRowsIdx 
    expandedColsIdx 
    expansionFactor 
    Coordinates{row=row2,col=col2} = totalDistance
    where
        [rowMin, rowMax] = sort [row1, row2]
        [colMin, colMax] = sort [col1, col2]
        numberOfExpandedRows = length $ filter (\x -> x > rowMin && x < rowMax) expandedRowsIdx
        numberOfExpandedCols = length $ filter (\x -> x > colMin && x < colMax) expandedColsIdx
        totalDistance = rowMax - rowMin + colMax - colMin + (expansionFactor * (numberOfExpandedRows + numberOfExpandedCols))


galaxiesCoordinates :: [[Char]] -> Int -> [Coordinates]
galaxiesCoordinates [] _ = []
galaxiesCoordinates (dataRow:mapRemaining) curRow = thisRowCoordinates ++ galaxiesCoordinates mapRemaining (curRow + 1)
    where
        thisRowCoordinates = map (\x-> Coordinates{row=curRow,col=x}) $ elemIndices '#' dataRow

expandSpace space = expandRows $ transpose $ expandRows space

expandRows :: [[Char]] -> [[Char]]
expandRows [] = []
expandRows (spaceRow:inputSpace) = if all (=='.') spaceRow
    then spaceRow:spaceRow:expandRows inputSpace
    else spaceRow:expandRows inputSpace