module Solutions.Y2025.Day9
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.Geometry2D
import Utils.List

data Rectangle = Rectangle
    { p1 :: Point
    , p2 :: Point
    , area :: Int
    } deriving(Eq, Show)

generateRectangles = generatePairsCommutative (\p1 p2-> Rectangle{p1=p1, p2=p2, area=rectangleArea p1 p2})


part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ maximum $ map area rectangles
    where
        parsed = map (parsePoint2 ',') $ lines inputStr
        rectangles = generateRectangles parsed


part2 :: [Char] -> Maybe Int
part2 inputStr = Nothing
