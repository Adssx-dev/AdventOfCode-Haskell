
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

main = do
    handle <- openFile "data/2023/Day3.txt" ReadMode
    contents <- hGetContents handle

    print $ parseData (lines contents) 0
    --print s

    hClose handle

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
parseLine (x:xs) maybeVal row col allValues allSymbols
    | isDigit x     = parseLine xs (Just newVal) row (col+1) allValues allSymbols
    | otherwise     = parseLine xs maybeVal row (col+1) allValues (newSymbol:allSymbols)
    where
        newVal = loadNumber maybeVal (read [x] :: Int) row col
        newSymbol = loadSymbol x row col


loadNumber Nothing val row col = Value{val=val, valCoords=[Coordinates{row,col}]}
loadNumber (Just previousValue) newValue row col = Value{val=(10 * val previousValue) + newValue, valCoords=Coordinates{row,col}:valCoords previousValue}

loadSymbol x row col = Symbol {symb=x, symbCoords=Coordinates{row, col}}