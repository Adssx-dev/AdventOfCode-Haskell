module Solutions.Y2017.Day1
( part1
, part2
) where
import System.IO
import Debug.Trace
import Data.List
import Data.Char

data Coordinates = Coordinates {row :: Int, col :: Int} deriving (Show, Eq, Ord)

main = do
    handle <- openFile "data/2017/Day1.txt" ReadMode
    contents <- hGetContents handle

    print $ part1 contents
    print $ part2 contents

    hClose handle

part1 inputStr = Just $ sum $ map (\x -> fst x * (snd x - 1)) $ groupCount (numList ++ takeWhile (== last numList) numList)
    where
        numList = map (\x -> read [x]) inputStr :: [Int]


part2 inputStr = Just $ sum $ map fst $ filter (uncurry (==)) zipped
    where
        numList = map (\x -> read [x]) inputStr :: [Int]
        zipped = zip numList (part2 ++ part1)
        (part1, part2) = splitAt (length numList `div` 2) numList


groupCount list = zip (map head grouping) (map length grouping)
    where
        grouping = group list