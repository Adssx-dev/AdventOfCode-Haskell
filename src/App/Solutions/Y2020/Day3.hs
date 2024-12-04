module Solutions.Y2020.Day3
( part1
, part2
) where
import System.IO
import Debug.Trace

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ treesEncounteredDuringSlide (lines inputStr) 3 1 0


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ product $ map (\s -> uncurry (treesEncounteredDuringSlide (lines inputStr)) s 0) speeds
    where 
        speeds = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

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
