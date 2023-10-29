import System.IO  
import Debug.Trace


main = do  
    handle <- openFile "data/2020/Day3.txt" ReadMode  
    --handle <- openFile "data/2020/Test.txt" ReadMode  
    contents <- hGetContents handle  
    print $ treesEncounteredDuringSlide (lines contents) 3 0
    hClose handle  

treesEncounteredDuringSlide :: [[Char]] -> Int -> Int -> Int
treesEncounteredDuringSlide [] _ _ = 0
treesEncounteredDuringSlide treeMap xSpeed xPosition = currentIsTree + treesEncounteredDuringSlide (tail treeMap) xSpeed (xPosition + xSpeed)
    where
        mapWidth = length $ head treeMap
        currentIsTree = trace (show xPosition) fromEnum ((head treeMap !! (xPosition `rem` mapWidth)) == '#')