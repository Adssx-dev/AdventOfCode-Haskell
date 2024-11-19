module Solutions.Y2023.Day8
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char

import qualified Data.Map as Map

part1 inputStr = Just $ browseTree tree directions "AAA" 0
    where
        allLines = lines inputStr
        directions = cycle $ head allLines
        tree = Map.fromList $ loadTree (tail $ tail allLines) []

part2 inputStr = Just $ foldr lcm 1 cycleLengths
    where
        allLines = lines inputStr
        directions = cycle $ head allLines
        tree = Map.fromList $ loadTree (tail $ tail allLines) []
        startingNodes = filter (endsWithLetter 'A') $ Map.keys tree
        cycleLengths = map (findCycleLength tree directions 0) startingNodes

-- Calculate the cycle length from a starting node to a node ending by "Z"
findCycleLength :: Map.Map String ([Char], [Char]) -> [Char] -> Int -> String -> Int
findCycleLength _ _ count (_:_:'Z':_) = count
findCycleLength tree (currentDirection:nextDirections) count node = findCycleLength tree nextDirections (count + 1) (getNextNode tree currentDirection node) 




endsWithLetter :: Char -> String -> Bool
endsWithLetter letter str = last str == letter

getNextNode :: Map.Map String ([Char], [Char]) -> Char -> String -> String
getNextNode tree currentDirection node = if currentDirection == 'L'
            then fst children
            else snd children
    where
        children = fromMaybe ("", "") $ Map.lookup node tree

browseTree :: Map.Map String ([Char], [Char]) -> [Char] -> String -> Int -> Int
browseTree _ _ "ZZZ" count = count
browseTree tree (currentDirection:nextDirections) node count = browseTree tree nextDirections nextNode (count + 1)
    where
        children = fromMaybe ("", "") $ Map.lookup node tree
        nextNode = if currentDirection == 'L'
            then fst children
            else snd children


loadTree :: [String] -> [(String, ([Char], [Char]))] -> [(String, ([Char], [Char]))]
loadTree [] tree = tree
loadTree (line:nextLines) tree = loadTree nextLines (newNode:tree)
    where
        [source, "=", childLRaw, childRRaw] = words line
        childL = tail $ init childLRaw
        childR = init childRRaw
        newNode = (source, (childL, childR))


