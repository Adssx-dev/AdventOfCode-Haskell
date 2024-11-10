import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char

import qualified Data.Map as Map
import Text.PrettyPrint (Style(lineLength))

main = do
    handle <- openFile "data/2023/Day12.txt" ReadMode
    contents <- hGetContents handle

    --print $ length $ generateValidLines "???.###" [1, 1,3]
    print $ part1 $ lines contents
    --print $ part2 $ lines contents


    hClose handle

part1 lines = sum $ map (length . uncurry generateValidLines) parsedLines
    where
        parsedLines = map parseLine lines

parseLine line = (pat, cluster)
    where
        [pat, clusterStr] = words line
        cluster = map read $ splitOn (==',') clusterStr :: [Int]



generateValidLines :: [Char] -> [Int] -> [[Char]]
generateValidLines reference clusters = validLines
    where 
        lineLength = length reference
        validLines = filter (compareLines reference) $ generateAllPossibleLines clusters lineLength

compareLines [] [] = True
compareLines (c1:l1) (c2:l2) = case (c1, c2) of
    ('.', '#') -> False
    ('#', '.') -> False
    (_, _) -> compareLines l1 l2



generateAllPossibleLines clusters lineLength = map (generateLine clusters) spaces
    where
        spaces = generateAllPossibleSpaces clusters lineLength


generateLine [] [s] = replicate s '.'
generateLine (c:clusters) (s:spaces) = replicate s '.' ++ replicate c '#' ++ generateLine clusters spaces


generateAllPossibleSpaces :: [Int] -> Int -> [[Int]]
generateAllPossibleSpaces clusters lineLength = possibleSpaces
    where
        expectedSum = lineLength - sum clusters
        spacesCount = length clusters + 1
        possibleSpaces = filter (\x -> 0 `notElem` tail (init x)) $ listsBoundLenSum (lineLength - sum clusters) spacesCount expectedSum

-- thanks https://www.reddit.com/r/haskell/comments/pdzg8t/generate_all_list_of_integers_01000_of_n_length/
--                  Allowed Values -> max length -> sum
listsBoundLenSum :: Int -> Int -> Int -> [[Int]]
listsBoundLenSum _ 0 0 = return []
listsBoundLenSum _ 0 _ = []
listsBoundLenSum b l s = do
    nxt <- [0 .. min b s]
    (nxt :) <$> listsBoundLenSum b (l - 1) (s - nxt)


splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'