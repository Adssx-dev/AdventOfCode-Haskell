module Solutions.Y2020.Day5
( part1
, part2
) where
import System.IO
import Debug.Trace
import Data.List

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ maximum passesIds
    where
        passes = lines inputStr
        passesIds = sort $ map (parseBoardingPass 0) passes


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ findSeat passesIds [1..1024]
    where
        passes = lines inputStr
        passesIds = sort $ map (parseBoardingPass 0) passes

findSeat passesIds possibleSeats = case result of
    Nothing -> 0
    Just value -> candidates !! value
    where
        candidates = map (+1) $ elemIndices True $ map (`notElem` passesIds) possibleSeats
        result = elemIndex True $ map (\x -> (x - 1) `elem` passesIds && (x + 1) `elem` passesIds) candidates

parseBoardingPass :: Int -> [Char] -> Int
parseBoardingPass value [] = value
parseBoardingPass value pass = parseBoardingPass (value * 2 + currentValue) (tail pass)
    where currentValue = case head pass of
            'B' -> 1
            'R' -> 1
            _ -> 0
