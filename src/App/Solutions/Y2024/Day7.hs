module Solutions.Y2024.Day7
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List



part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum $ map fst $ filter (isSolvable operators) equations
    where
        operators = [(+), (*)]
        equations = map parseEquation $ lines inputStr


isSolvable :: [Int -> Int -> Int] ->  (Int, [Int]) -> Bool
isSolvable operators (target, numbers) = elem target $ map (solveEquation numbers) candidates
    where
        candidates = generateOperationCandidates (length numbers - 1) operators

solveEquation :: [Int] -> [Int -> Int -> Int] -> Int
solveEquation [x] [c] = error "Error in number of operators not matching number of numbers"
solveEquation [x] _ = x
solveEquation (n1:n2:numbers) (op:operators) = solveEquation (tmp:numbers) operators
    where
        tmp = op n1 n2

-- solveEquation
--     where
--         operationsCandidates = 
-- [(+), (*)]
generateOperationCandidates :: Int -> [a] -> [[a]]
generateOperationCandidates 0 _ = [[]]
generateOperationCandidates count elements = concatMap (\x -> appendToAllLists x $ generateOperationCandidates (count - 1) elements) elements
    where
        appendToAllLists a = map (a:)


parseEquation inputStr = (target, numbers)
    where
        (target:numbers) = map read $ words $ filter (/= ':') inputStr :: [Int]

part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ sum $ map fst $ filter (isSolvable operators) equations
    where
        operators = [(+), (*), concatenateInts]
        equations = map parseEquation $ lines inputStr

-- New function to apply for the part 2 : concatenates 2 integers
concatenateInts :: Int -> Int -> Int
concatenateInts a b = a  * 10 ^ (floor (logBase 10 (fromIntegral b)) + 1) +  b