{-# LANGUAGE NamedFieldPuns #-}
module Solutions.Y2023.Day3
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Language.Haskell.TH (newtypeStrategy)


data Coordinates = Coordinates {row :: Int, col :: Int} deriving (Show, Eq, Ord)
data Value = Value {val :: Int, valCoords :: [Coordinates]} deriving (Show, Eq, Ord)
data Symbol = Symbol {symb :: Char, symbCoords :: Coordinates} deriving (Show, Eq, Ord)

part1 inputStr = Just $ sum $ map val keptValues
    where
        (values, symbols) = parseData (lines inputStr) 0
        symbolCoordinates = map symbCoords symbols
        keptValues = filter (any (`elem` symbolCoordinates) . getBoundingBox) values

part2 inputStr = Just $ sum $ map (getGearProduct values) gears 
    where
        (values, symbols) = parseData (lines inputStr) 0
        gears = filter (\x -> symb x == '*') symbols


getGearProduct values gear  = gearProduct
    where
        gearCoordinates = symbCoords gear
        adjacentValues = map val $ filter (\x -> gearCoordinates `elem` getBoundingBox x) values
        gearProduct = if length adjacentValues < 2
            then 0
            else product adjacentValues



getBoundingBox :: Value -> [Coordinates]
getBoundingBox val = [Coordinates{row=y, col=x} | x <- [minimum cols - 1 .. (maximum cols + 1)],  y <- [(minimum rows - 1)..(maximum rows + 1)]]
    where
        rows = map row $ valCoords val
        cols = map col $ valCoords val


parseData :: [[Char]] -> Int -> ([Value], [Symbol])
parseData [] _ = ([], [])
parseData (line:xs) row = (currentValues ++ nextValues, currentSymbols ++ nextSymbols)
    where
        (currentValues, currentSymbols) = parseLine line Nothing row 0 [] []
        (nextValues, nextSymbols) = parseData xs (row + 1)

parseLine :: [Char] -> Maybe Value -> Int -> Int -> [Value] -> [Symbol] -> ([Value], [Symbol])
parseLine [] Nothing _ _ allValues allSymbols = (allValues, allSymbols)
parseLine [] (Just v) _ _ allValues allSymbols = (v:allValues, allSymbols)
parseLine ('.':xs) maybeVal row col allValues allSymbols = parseLine xs Nothing row (col+1) newAllValues allSymbols
    where
        newAllValues = case maybeVal of
            Nothing -> allValues
            Just v -> v:allValues
parseLine (x:xs) Nothing row col allValues allSymbols
    | isDigit x     = parseLine xs (Just newVal) row (col+1) allValues allSymbols
    | otherwise     = parseLine xs Nothing row (col+1) allValues (newSymbol:allSymbols)
    where
        newVal = loadNumber Nothing (read [x] :: Int) row col
        newSymbol = loadSymbol x row col

parseLine (x:xs) (Just v) row col allValues allSymbols
    | isDigit x     = parseLine xs (Just newVal) row (col+1) allValues allSymbols
    | otherwise     = parseLine xs Nothing row (col+1) (v:allValues) (newSymbol:allSymbols)
    where
        newVal = loadNumber (Just v) (read [x] :: Int) row col
        newSymbol = loadSymbol x row col

loadNumber Nothing val row col = Value{val=val, valCoords=[Coordinates{row,col}]}
loadNumber (Just previousValue) newValue row col = Value{val=(10 * val previousValue) + newValue, valCoords=Coordinates{row,col}:valCoords previousValue}

loadSymbol x row col = Symbol {symb=x, symbCoords=Coordinates{row, col}}