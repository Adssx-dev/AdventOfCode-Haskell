{-# LANGUAGE RecordWildCards #-}
module Solutions.Y2024.Day13
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.Matrix
import Utils.Geometry2D

data Machine = Machine{mat::Matrix2, vec :: Vector}

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum $ map tokenCost machines
    where
        machines = parse (lines inputStr) []

parse :: [String] -> [Machine] -> [Machine]
parse [] machines = machines
parse ([]:xs) machines = parse xs machines
parse (a:b:prize:xs) machines = parse xs (Machine{mat=mat, vec=vec}:machines)
    where
        readX str = read $ signFilter $ takeWhile (/= ',') $ drop 1 $ dropWhile (/= 'X') str :: Int
        readY str = read $ signFilter $ drop 1 $ dropWhile (/= 'Y') str :: Int
        signFilter = filter (\x -> x /= '+' && x /= '=')
        ax = readX a
        ay = readY a
        bx = readX b
        by = readY b
        tx = readX prize
        ty = readY prize
        mat = Matrix2{a = ax, b = bx, c = ay, d = by}
        vec = Vector{dx = tx, dy = ty}

tokenCost Machine{..} = case solveLinearEquation mat vec of
    Nothing -> 0
    Just sol -> dx sol * 3 + dy sol


part2 :: [Char] -> Maybe Int
part2 inputStr =  Just $ sum $ map tokenCost machinesFurther
    where
        machines = parse (lines inputStr) []
        machinesFurther = map (\Machine{..} -> Machine{mat=mat, vec=Vector{dx=dx vec + 10000000000000, dy = dy vec + 10000000000000}}) machines
