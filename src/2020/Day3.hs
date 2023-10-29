import System.IO
import Debug.Trace


main = do
    handle <- openFile "data/2020/Day3.txt" ReadMode
    --handle <- openFile "data/2020/Test.txt" ReadMode  
    contents <- hGetContents handle

    -- Part 1
    print $ treesEncounteredDuringSlide (lines contents) 3 1 0

    -- Part 2
    let speeds = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    print $ product $ map (\s -> uncurry (treesEncounteredDuringSlide (lines contents)) s 0) speeds

    hClose handle
    

treesEncounteredDuringSlide :: [[Char]] -> Int -> Int -> Int -> Int
treesEncounteredDuringSlide [] _ _ _ = 0
treesEncounteredDuringSlide treeMap xSpeed ySpeed xPosition = currentIsTree + treesEncounteredDuringSlide remainingMap xSpeed ySpeed (xPosition + xSpeed)
    where
        mapWidth = length $ head treeMap
        currentIsTree = fromEnum ((head treeMap !! (xPosition `rem` mapWidth)) == '#')
        -- Needed because in the lase case we would go over the end of the map with the "tail $ tail" so we check the right operation to do
        remainingMap = case (ySpeed, length treeMap) of
            (1, _) -> tail treeMap
            (2, 1) -> []
            (2, _) -> tail $ tail  treeMap
            _ -> []
