module Solutions.Y2025.Day3
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Data.Char (digitToInt)


part1 :: [Char] -> Maybe Int
part1 inputStr = trace (show $ map solveBank banks) Just $ sum $ map solveBank banks
    where
        banks = parseInput inputStr

solveBank (val1:val2:remaining) = bestPair val1 val2 (val2:remaining)

bestPair :: Int -> Int -> [Int] -> Int
bestPair digit1 digit2 [] = digit1 * 10 + digit2
bestPair digit1 digit2 [val] = digit1 * 10 + max digit2 val
bestPair digit1 digit2 (val1:val2:remaining)
  | val1 > digit1 = bestPair val1 val2 (val2:remaining)
  | val1 > digit2 = bestPair digit1 val1 (val2:remaining)
  | otherwise = bestPair digit1 digit2 (val2:remaining)

parseInput :: String -> [[Int]]
parseInput = map parseBank . lines

parseBank :: String -> [Int]
parseBank b = map digitToInt b :: [Int]

part2 :: [Char] -> Maybe Int
part2 inputStr = Nothing
