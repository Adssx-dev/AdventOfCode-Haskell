module Solutions.Y2025.Day6
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List


type Operand = Int
type Operator = (Int -> Int -> Int)

parseInput :: String -> ([[Operand]], [Operator])
parseInput inputStr = (map (map read . words) operandLines :: [[Operand]], map parseOperator $ words operatorLine)
    where
        lns = lines inputStr
        operandLines = init lns
        operatorLine = last lns
        parsedOperands = map words operandLines

parseOperator "+" = (+)
parseOperator "*" = (*)

solveOneProblem :: [Operand] -> Operator -> Int
solveOneProblem operands operator = foldl1 operator operands

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum res
    where
        (operands, operators) = parseInput inputStr
        res = zipWith solveOneProblem (transpose operands) operators





part2 :: [Char] -> Maybe Int
part2 inputStr = Nothing
