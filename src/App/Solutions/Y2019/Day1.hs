module Solutions.Y2019.Day1
( part1
, part2
) where
import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char


fuelNeeded :: Int -> Int
fuelNeeded mass =  -2 + floor (fromIntegral mass / 3)

compoundFuelNeeded ::  Int -> Int
compoundFuelNeeded mass = currentFuel + compoundFuel
    where
        currentFuel = max (fuelNeeded mass) 0
        compoundFuel = if currentFuel <= 0
            then 0
            else compoundFuelNeeded currentFuel

part1 inputStr = Just $ sum values
    where 
        values = map  (fuelNeeded . read) (lines inputStr) :: [Int]

part2 inputStr = Just $ sum values
    where 
        values = map  (compoundFuelNeeded . read) (lines inputStr):: [Int]

