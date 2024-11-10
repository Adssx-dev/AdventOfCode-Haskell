module Solutions.Y2015.Day2
( part1
, part2
) where


import System.IO
import Debug.Trace
import Data.List

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum $ map (computeArea . parsePresents) $ lines inputStr


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ sum $ map (computeRibbonLength . parsePresents) $ lines inputStr

computeArea :: (Int, Int, Int) -> Int
computeArea (l, w, h) = minimum sides + 2 * sum sides 
    where
        sides = [l * w, w * h, h * l]

computeRibbonLength :: (Int, Int, Int) -> Int
computeRibbonLength (l, w, h) = volume + minimum perimeters
    where
        perimeters = [2*l + 2*w, 2*w + 2*h, 2*h + 2*l]
        volume = l * w * h

parsePresents :: [Char] -> (Int, Int, Int)
parsePresents sizeStr = (read (splitted !! 0) :: Int, read (splitted !! 1) :: Int, read (splitted !! 2) ::Int)
    where
        splitted = splitOn (=='x') sizeStr

-- Split a list on a given predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'