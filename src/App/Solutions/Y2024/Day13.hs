{-# LANGUAGE RecordWildCards #-}
module Solutions.Y2024.Day13
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.Matrix

data Machine = Machine{mat::Matrix2, vec :: Vector2f}

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum $ map tokenCost machines
    where 
        machines = parse (lines inputStr) []

parse :: [String] -> [Machine] -> [Machine]
parse [] machines = machines
parse ([]:xs) machines = parse xs machines
parse (a:b:prize:xs) machines = parse xs (Machine{mat=mat, vec=vec}:machines)
    where
        readX str = read $ signFilter $ takeWhile (/= ',') $ drop 1 $ dropWhile (/= 'X') str :: Double
        readY str = read $ signFilter $ drop 1 $ dropWhile (/= 'Y') str :: Double
        signFilter = filter (\x -> x /= '+' && x /= '=')
        ax = readX a
        ay = readY a
        bx = readX b
        by = readY b
        tx = readX prize
        ty = readY prize
        mat = Matrix2{a = ax, b = bx, c = ay, d = by}
        vec = Vector2f{dx = tx, dy = ty}

-- trace (show (a, b)) $
tokenCost :: Machine -> Int
tokenCost Machine{..} =  trace (show (a, b)) $ if isInt a && isInt b
    then 3 * round a + round b
    else 0
    where
        solution = mul (inv mat) vec
        a = dx solution
        b = dy solution

-- isSolution Matix2{..} Vector2f{..} = 
--     where
--         dxInt = round

isInt :: Double -> Bool
isInt x = isIntInt x 1

isIntInt :: (Integral a, RealFrac b) => b -> a -> Bool
isIntInt x n = (round $ 10^(fromIntegral n)*(x-(fromIntegral $ round x)))==0

part2 :: [Char] -> Maybe Int
part2 inputStr = trace (show $ map tokenCost machinesFurther) $  Just $ sum $ map tokenCost machinesFurther
    where 
        machines = parse (lines inputStr) []
        machinesFurther = map (\Machine{..} -> Machine{mat=mat, vec=Vector2f{dx=(dx vec)+10000000000000, dy = (dy vec)+10000000000000}}) machines
