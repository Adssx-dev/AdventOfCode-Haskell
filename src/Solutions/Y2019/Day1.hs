import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char


main = do
    handle <- openFile "data/2019/Day1.txt" ReadMode
    contents <- hGetContents handle


    print $ part1 $ lines contents
    print $ part2 $ lines contents

    hClose handle

fuelNeeded :: Int -> Int
fuelNeeded mass =  -2 + floor (fromIntegral mass / 3)

compoundFuelNeeded ::  Int -> Int
compoundFuelNeeded mass = currentFuel + compoundFuel
    where
        currentFuel = max (fuelNeeded mass) 0
        compoundFuel = if currentFuel <= 0
            then 0
            else compoundFuelNeeded currentFuel

part1 lines = sum values
    where 
        values = map  (fuelNeeded . read) lines :: [Int]

part2 lines = sum values
    where 
        values = map  (compoundFuelNeeded . read) lines :: [Int]

