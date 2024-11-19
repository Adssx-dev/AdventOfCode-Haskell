module Solutions.Y2020.Day13
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe


part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ solve (read targetStr :: Int) (head busLines)
    where 
        (targetStr:busLines) = lines inputStr


part2 :: [Char] -> Maybe Int
part2 inputStr = Nothing

solve target allBusLines = uncurry (*) bestTime
    where
        busLines = filter (/= "x") $ splitOn (==',') allBusLines
        busLinesInt = map read busLines :: [Int]
        times = map (\x -> (firstMultiplierWaitTime target x, x)) busLinesInt
        bestTime = minimumBy (compare `on` fst) times

firstMultiplierWaitTime :: Int -> Int -> Int
firstMultiplierWaitTime target base = waitTime
    where
        nextTime = base * ((target `div` base) + 1)
        waitTime = nextTime - target

-- Split a list on a given predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'
