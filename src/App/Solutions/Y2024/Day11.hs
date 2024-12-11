module Solutions.Y2024.Day11
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import qualified Data.Map as Map
import Utils.List


part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ fromIntegral $ go Map.empty values 25 0
    where
        values = map read (words inputStr) :: [Integer]
        x = countBlink Map.empty (Just 0) 25

go mem [] count tot = tot
go mem (x:xs) count tot = go newMem xs count (tot + newTotal)
    where
        (newMem, newTotal) = countBlink mem (Just x) count


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ fromIntegral $ go Map.empty values 75 0
    where
        values = map read (words inputStr) :: [Integer]

countBlink :: Map.Map (Integer, Int) Integer -> Maybe Integer -> Int -> (Map.Map (Integer, Int) Integer, Integer)
countBlink mem Nothing _ = (mem, 0)
countBlink mem _ 0 = (mem, 1)
countBlink mem (Just value) rem = case Map.lookup (value, rem) mem of
    Just value -> (mem, value)
    Nothing -> (newGlobalMap, result)
        where
            newValues = nextBlink value
            (newMem1, res1) = countBlink mem (safeHead newValues) $ rem - 1
            (newMem2, res2) = countBlink newMem1 (safeHead $ drop 1 newValues) $ rem - 1
            result = res1 + res2
            newGlobalMap = Map.insert (value, rem) result newMem2


nextBlink :: Integer -> [Integer]
nextBlink value 
    | value == 0 = [1]
    | even numberOfDigits = [read $ take splitNumber valueAsStr, read $ take splitNumber $ drop splitNumber valueAsStr]
    | otherwise = [value * 2024]
    where 
        valueAsStr = show value
        numberOfDigits = ceiling $ logBase 10 (fromIntegral $ value + 1)
        splitNumber = numberOfDigits `div` 2

