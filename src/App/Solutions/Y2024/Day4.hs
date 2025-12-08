module Solutions.Y2024.Day4
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List

import Utils.Matrix
import Utils.Geometry2D
import qualified Data.Map as Map 

-- Part 1 treats as a list of lists and extracts rows, columns and diagonals in both ways
-- Part 2 transforms into coordinates and considers all 'A's as candidates as they are in the center of crosses and checks surrounding values
part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ countPt1 mat
    where
        mat = lines inputStr

countPt1 :: [[Char]] -> Int
countPt1 mat = trace (show [sumLines, sumColumns, sumDiag1, sumDiag2]) sumLines + sumColumns + sumDiag1 + sumDiag2
    where
        lines = mat
        columns = transpose mat
        diag1 = diagonals mat
        diag2 = diagonals $ reverse mat
        sumLines = sum (map (count 0) lines)
        sumColumns = sum (map (count 0) columns)
        sumDiag1 = sum (map (count 0) diag1)
        sumDiag2 = sum $ map (count 0) diag2

-- Beware to put back the last letter as it can also be reused in the case of XMASAMX or SAMXMAS which counts of 2 
count :: Int -> [Char] -> Int
count accum [] = accum
count accum ('X':'M':'A':'S':xs) = count (accum + 1) ('S':xs) 
count accum ('S':'A':'M':'X':xs) = count (accum + 1) ('X':xs)
count accum (x:xs) = count accum xs


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ length $ filter (isCross coordMap) candidates
    where
        coordMap = toCoordinateMap $ lines inputStr
        candidates = map fst $ filter (\(p, val) -> val =='A') $ Map.toList coordMap


-- Checks whether diagonals both contain one "M" and one "S"
isCross coordMap candidate = validDiag diag1 && validDiag diag2
    where
        point1 = Point{x= x candidate - 1, y= y candidate - 1} -- Top left
        point2 = Point{x= x candidate + 1, y= y candidate - 1} -- To right
        point3 = Point{x= x candidate + 1, y= y candidate + 1} -- Bottom right
        point4 = Point{x= x candidate - 1, y= y candidate + 1} -- Bottom left
        diag1 = (Map.lookup point1 coordMap, Map.lookup point3 coordMap) -- TL -> BR
        diag2 = (Map.lookup point2 coordMap, Map.lookup point4 coordMap) -- TR -> BL

-- Helper function to validate a diagonal no matter the order of the elements
validDiag (Just 'S', Just 'M') = True
validDiag (Just 'M', Just 'S') = True
validDiag _ = False


