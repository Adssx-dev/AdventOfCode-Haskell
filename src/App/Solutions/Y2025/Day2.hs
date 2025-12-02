module Solutions.Y2025.Day2
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.List
import Utils.Math

type Range = (Int, Int)

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum filtered
    where
        ranges = parseInput inputStr
        allIds = generateAllIds ranges
        filtered = filter (not . isValidId) allIds


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ sum filtered
    where
        ranges = parseInput inputStr
        allIds = generateAllIds ranges
        filtered = filter (not . isValidIdPt2) allIds

generateAllIds :: [Range] -> [Int]
generateAllIds = concatMap generateRangeIds

generateRangeIds :: Range-> [Int]
generateRangeIds (start, end) = [start..end]

isValidId :: Int -> Bool
isValidId num = odd digitCount || (higherDigits /= lowerDigits)
    where
        digitCount = numDigits num
        exponent = 10 ^ div digitCount 2
        higherDigits = div num exponent
        lowerDigits = num - (higherDigits * exponent)

isValidIdPt2 :: Int -> Bool
isValidIdPt2 num = all (isValidIdForDivisor num) divs
    where
        digitCount = numDigits num
        divs = map (div digitCount ) $ divisorCandidates digitCount


isValidIdForDivisor num div =  not $ all (== head values) values
    where
        digitCount = numDigits num
        values = splitNumber num digitCount div

parseInput :: [Char] -> [Range]
parseInput l =
    let
        rangesStr = splitOn (== ',') l
    in
        map parseRange rangesStr

parseRange :: [Char] -> Range
parseRange range =
    let
        [start, end] = splitOn (=='-') range
    in
        (read start :: Int, read end :: Int)

splitNumber :: Int -> Int -> Int -> [Int]
splitNumber value numDigits divisor = if (numDigits - divisor) < 0
    then []
    else higherDigits:splitNumber lowerDigits (numDigits - divisor) divisor
    where
        exponent = 10 ^ (numDigits - divisor)
        higherDigits = div value exponent
        lowerDigits = value - (higherDigits * exponent)

divisorCandidates :: Int -> [Int]
divisorCandidates x = tail $ divisors x