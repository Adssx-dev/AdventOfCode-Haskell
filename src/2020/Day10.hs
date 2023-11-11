import System.IO
import Debug.Trace
import Data.List


main = do
    handle <- openFile "data/2020/Day10.txt" ReadMode
    contents <- hGetContents handle

    print $ part1 $ lines contents
    print $ part2 $ lines contents

    hClose handle


-- Only the number of successives ones matter for each group of ones
-- Number of possible path depending on the number of successive ones :
-- 1 -> 1
-- 2 -> 2
-- 3 -> 4
-- 4 -> 7
-- More -> IDK
part2 lines = product products
    where
        intialNumberList = nub $ sort $ map read lines :: [Int]
        numberList = 0 : intialNumberList ++  [maximum intialNumberList + 3]
        differences = map length $ filter (\x -> head x == 1) $ group $  zipWith (\ a b -> b - a) (init numberList) (tail numberList)
        products = map (\x -> [1, 2, 4, 7] !! (x-1)) differences

part1 lines = product differences
    where
        intialNumberList = nub $ sort $ map read lines :: [Int]
        numberList = 0 : intialNumberList ++  [maximum intialNumberList + 3]
        differences = map length $ group $ sort $ zipWith (\ a b -> b - a) (init numberList) (tail numberList)
