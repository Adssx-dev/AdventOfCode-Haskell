module Solutions.Y2025.Day3
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Data.Char (digitToInt)
import Utils.List
import Utils.Math
import Data.Function


-- Part 1
part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum $ map (intArrayToValue . solveBank 2) banks
    where
        banks = parseInput inputStr

-- basic way of solving part 1:
-- solveBank (val1:val2:remaining) = bestPair val1 val2 (val2:remaining)

-- bestPair :: Int -> Int -> [Int] -> Int
-- bestPair digit1 digit2 [] = digit1 * 10 + digit2
-- bestPair digit1 digit2 [val] = digit1 * 10 + max digit2 val
-- bestPair digit1 digit2 (val1:val2:remaining)
--   | val1 > digit1 = bestPair val1 val2 (val2:remaining)
--   | val1 > digit2 = bestPair digit1 val1 (val2:remaining)
--   | otherwise = bestPair digit1 digit2 (val2:remaining)

-- Part 2
part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ sum $ map (intArrayToValue . solveBank 12) banks
    where
        banks = parseInput inputStr

findBestCandidate :: [Int] -> [Int]
findBestCandidate list = maximumBy compareArrays candidates
    where
        candidates = generateCandidates list

compareArrays [] [] = EQ
compareArrays [] x = error "Arrays have incompatible size"
compareArrays x [] = error "Arrays have incompatible size"
compareArrays (a1:a1rem) (a2:a2rem) 
    | a1 > a2 = GT
    | a1 < a2 = LT
    | otherwise = compareArrays a1rem a2rem

generateCandidates :: [Int] -> [[Int]]
generateCandidates list = map (`removeAt` list) [0..(length list - 1)]

solveBank :: Int -> [Int] -> [Int]
solveBank targetSize = until (\x -> length x == targetSize) findBestCandidate


-- General 
parseInput :: String -> [[Int]]
parseInput = map parseBank . lines

parseBank :: String -> [Int]
parseBank b = map digitToInt b :: [Int]
