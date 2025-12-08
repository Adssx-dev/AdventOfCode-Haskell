module Solutions.Y2024.Day8
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.Matrix
import qualified Data.Map as Map
import Utils.Tuple
import Utils.Geometry2D
import Utils.List

part1 :: [Char] -> Maybe Int
part1 inputStr =  Just $ length $ nub allAntinodes
    where
        (limX, limY, antennas) = parseAntennas inputStr
        allAntinodes = concatMap (getAntinodes limX limY getAntinodesPair . snd)  antennas

parseAntennas :: String -> (Int, Int, [(Char, [Point])])
parseAntennas inputStr = (limX, limY, allAntennas)
    where
        mapAsList = Map.toList $
            toCoordinateMap $
            lines inputStr
        limX = maximum $ map (x . fst) mapAsList
        limY = maximum $ map (y . fst) mapAsList
        allAntennas = map (\gr -> (fst $ head gr, map snd gr)) $
            groupBy (\a b -> fst a == fst b) $
            sortBy (\ a b -> compare (fst a) (fst b)) $
            map reverseTuple $ filter (\x -> snd x /= '.') mapAsList

getAntinodes :: Int -> Int -> ((Point, Point) -> [Point]) -> [Point] -> [Point]
getAntinodes limX limY pairFunction points = filter (inBounds limX limY) antinodesCandidates
    where
        antennaPairs = pairs points
        antinodesCandidates = concatMap pairFunction antennaPairs

inBounds :: Int -> Int -> Point -> Bool
inBounds limX limY  Point{x=x, y=y} = y <= limY && x <= limX && x >= 0 && y >= 0

getAntinodesPair :: (Point, Point) -> [Point]
getAntinodesPair (p1, p2) = [apply vec p2, apply (reverseVector vec) p1]
    where
        vec = getTranslationVector p1 p2

getAntinodesPairUntilEnd :: Int -> Int -> (Point, Point) -> [Point]
getAntinodesPairUntilEnd limX limY (p1, p2) =  nextPairs limX limY p1 vec ++ nextPairs limX limY p2 revVec
    where
        vec = getTranslationVector p1 p2
        revVec = reverseVector vec

nextPairs :: Int -> Int -> Point -> Vector -> [Point]
nextPairs limX limY point vec = if inBounds limX limY  nextPoint
    then nextPoint : nextPairs limX limY nextPoint vec
    else []
    where
        nextPoint = apply vec point

part2 :: [Char] -> Maybe Int
part2 inputStr =  Just $ length $ nub allAntinodes
    where
        (limX, limY, antennas) = parseAntennas inputStr
        allAntinodes = concatMap (getAntinodes limX limY (getAntinodesPairUntilEnd limX limY) . snd)  antennas
