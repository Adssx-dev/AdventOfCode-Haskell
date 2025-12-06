module Solutions.Y2025.Day6
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.List


type Operand = Int
type Operator = (Int -> Int -> Int)

-- Parsing
parseInputPart1 :: String -> ([[Operand]], [Operator])
parseInputPart1 inputStr = (map (map read . words) operandLines :: [[Operand]], map parseOperator $ words operatorLine)
    where
        lns = lines inputStr
        operandLines = init lns
        operatorLine = last lns
        parsedOperands = map words operandLines



parseInputPart2 :: String -> [([Operand], Operator)]
parseInputPart2 inputStr = map parseOneLinePt2 cleaned
    where
        reordered = reverse $ transpose $ lines inputStr
        grouped = splitOn isStringBlank reordered
        cleaned =  map (concatMap (filter (not.isStringBlank) . (\g -> [init g, [last g]]))) grouped

parseOneLinePt2 :: [String] -> ([Operand], Operator)
parseOneLinePt2 strings = (operands, operator)
    where
        operands = map read (init strings) :: [Operand]
        operator = parseOperator $ last strings

parseOperator "+" = (+)
parseOperator "*" = (*)

isStringBlank :: String -> Bool
isStringBlank = all (==' ')

-- Solving
solveOneProblem :: [Operand] -> Operator -> Int
solveOneProblem operands operator = foldl1 operator operands

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum res
    where
        (operands, operators) = parseInputPart1 inputStr
        res = zipWith solveOneProblem (transpose operands) operators

part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ sum $ map (uncurry solveOneProblem) parsed
    where
        parsed = parseInputPart2 inputStr
