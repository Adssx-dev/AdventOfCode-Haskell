module Solutions.Y2023.Day6
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char


part1 inputStr = Just $ product possibilities
    where
        [timesStr, distancesStr] = lines inputStr
        times = map read (tail $ words timesStr) :: [Int]
        distances = map read (tail $ words distancesStr) :: [Int]
        zipped = zip (map fromIntegral times :: [Double]) (map fromIntegral distances :: [Double])
        possibilities = map (\(t, d) -> numberOfIntegerQradraticSolutions (-1) t (-d - 0.1)) zipped

part2 inputStr = Just result
    where
        [timesStr, distancesStr] = lines inputStr
        time = read (concat $ tail $ words timesStr) :: Double
        distance = read (concat $ tail $ words distancesStr) :: Double
        result =  numberOfIntegerQradraticSolutions (-1) time (-distance - 0.1)

numberOfIntegerQradraticSolutions :: Double -> Double -> Double -> Int
numberOfIntegerQradraticSolutions a b c =  floor largeRoot - ceiling smallRoot + 1
    where
        delta = b * b - 4 * a * c
        roots = [-(b - sqrt delta)/(2*a), -(b + sqrt delta)/(2*a)]
        smallRoot = minimum roots
        largeRoot = maximum roots
