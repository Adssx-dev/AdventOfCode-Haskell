import System.IO
import Debug.Trace
import Data.List


main = do
    handle <- openFile "data/2020/Day10.txt" ReadMode
    contents <- hGetContents handle

    print $ part1 $ lines contents

    hClose handle


part1 lines = product differences
    where
        intialNumberList = nub $ sort $ map read lines :: [Int]
        numberList = 0 : intialNumberList ++  [maximum intialNumberList + 3]
        differences = map length $ group $ sort $ zipWith (\ a b -> b - a) (init numberList) (tail numberList)
