import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char

import qualified Data.Map as Map  

main = do
    handle <- openFile "data/2023/Day8.txt" ReadMode
    contents <- hGetContents handle

    let allLines = lines contents
    let directions = cycle $ head allLines
    let tree = Map.fromList $ loadTree (tail $ tail allLines) []
    print $ browseTree tree directions "AAA" 0
    --print $ part1 (lines contents)
    --print $ part2 (lines contents)

    hClose handle

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


