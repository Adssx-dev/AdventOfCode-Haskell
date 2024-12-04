module Solutions.Y2018.Day1
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Data.Char
import Data.Set

data Coordinates = Coordinates {row :: Int, col :: Int} deriving (Show, Eq, Ord)

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum $ parseNumList inputStr


part2 :: [Char] -> Maybe Int
part2 inputStr = fmap fst firstDuplicate
    where
        cumulSum = scanl (+) 0 $ cycle $ parseNumList inputStr
        firstDuplicate = firstDuplicateElement cumulSum

parseNumList inputStr = Data.List.map (read . Data.List.filter (/= '+')) $ lines inputStr :: [Int]

firstDuplicateElement :: (Ord a1) => [a1] -> Maybe (a1, Int)
firstDuplicateElement list = firstDuplicateElementInternal list Data.Set.empty 0
    

firstDuplicateElementInternal :: (Ord a) => [a] -> Set a -> Int -> Maybe (a, Int)
firstDuplicateElementInternal [] _ _ = Nothing
firstDuplicateElementInternal (x:xs) set index = if Data.Set.member x set
    then Just (x, index)
    else firstDuplicateElementInternal xs (Data.Set.insert x set) $ index + 1