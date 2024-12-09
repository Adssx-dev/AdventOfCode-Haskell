{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Solutions.Y2024.Day9
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Data.Char
import Data.Maybe


part1 :: [Char] -> Maybe Int
part1 inputStr = Just sumProd
    where
        program = parseData inputStr False 0
        sumProd = sum $ zipWith (*) [0..] (compact program)

compact :: [Maybe Int] -> [Int]
compact x = compactInternal x (reverse x) (length x) 0 0

compactInternal :: [Maybe Int] -> [Maybe Int] -> Int -> Int -> Int -> [Int]
compactInternal _ _ totalValues forwardValues reverseValues
    | totalValues == forwardValues + reverseValues = []
compactInternal (Just x:xs) rev totalValues forwardValues reverseValues =  x: compactInternal xs rev totalValues (forwardValues + 1) reverseValues
compactInternal (Nothing:xs) (Nothing:rs) totalValues forwardValues reverseValues = compactInternal (Nothing:xs) rs totalValues forwardValues (reverseValues + 1)
compactInternal (Nothing:xs) (Just y:rs) totalValues forwardValues reverseValues = y:compactInternal xs rs totalValues (forwardValues + 1) (reverseValues + 1)

parseData :: [Char] -> Bool -> Int-> [Maybe Int]
parseData [] _ _ = []
parseData (x:xs) isEmptySpace id = currentIterList ++ parseData xs (not isEmptySpace) newId
    where
        num = digitToInt x
        currentIterList = if isEmptySpace
            then replicate num Nothing
            else replicate num (Just id)
        newId = if isEmptySpace
            then id
            else id + 1


part2 :: [Char] -> Maybe Int
part2 inputStr = Just sumProd
    where
        program = parseData inputStr False 0
        groupedProgram = map (\l@(x:xs) -> (x,length l)) $ group program
        revGroupedProgram = map (\l@(x:xs) -> (x,length l)) $ group $ reverse (catMaybes program)
        sumProd = sum $ zipWith (*) [0..] (compactWhole groupedProgram revGroupedProgram)

compactWhole :: [(Maybe Int, Int)] -> [(Int, Int)] -> [Int]
compactWhole [] _ = []
compactWhole ((Just x, num):xs) rem =   replicate num x ++ compactWhole xs (delete (x, num) rem)
compactWhole ((Nothing, num):xs) rem =  case candidate of
        [] -> replicate num 0 ++ compactWhole xs rem
        [(candVal, candNum)] | candNum < num -> replicate candNum candVal ++ compactWhole ((Nothing, num - candNum):newXs) newRem
        [(candVal, candNum)] | candNum == num -> replicate candNum candVal ++ compactWhole newXs newRem
    where
        candidate = take 1 $ filter (\x -> num >= snd x) rem
        newRem = delete (head candidate) rem
        candMaybe = (Just $ fst $ head candidate, snd $ head candidate)
        candNothing= (Nothing, snd $ head candidate)
        newXs = map (\x -> if x == candMaybe then candNothing else x) xs


