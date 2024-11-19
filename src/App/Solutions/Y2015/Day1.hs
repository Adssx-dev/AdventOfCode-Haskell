module Solutions.Y2015.Day1
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List


part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum $ map convertParenthesis inputStr


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ 1 + length (takeWhile (>=0) $ scanl1 (+) $ map convertParenthesis inputStr)


convertParenthesis :: Char -> Int
convertParenthesis '('  =  1
convertParenthesis ')'  = -1
convertParenthesis char = error $ "Invalid character in sequence" ++ [char]

