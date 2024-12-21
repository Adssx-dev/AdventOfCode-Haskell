{-# LANGUAGE RecordWildCards #-}
module Solutions.Y2024.Day14
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.Geometry

data Robot = Robot
    { pos :: Point
    , velocity :: Vector
    } deriving (Eq, Show, Ord)

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ countQuadrants gridWidth gridHeight (list !! 100)
    where
        robots = map parseRobot $ lines inputStr
        gridWidth = 101
        gridHeight = 103
        list = iterate (simulateRobots gridWidth gridHeight) robots

simulateRobots gridWidth gridHeight robots = map (step gridWidth gridHeight) robots

parseRobot botStr = Robot{pos=Point{x=x, y=y}, velocity=Vector{dx=dx, dy=dy}}
    where
        coordsStr = takeWhile (/= ' ') $ drop 2 botStr
        veloccityStr = takeWhile (/= ' ') $ drop 2 $ dropWhile (/= 'v') botStr
        x = read $ takeWhile (/= ',') coordsStr :: Int
        y = read $ drop 1 $ dropWhile (/= ',') coordsStr :: Int
        dx = read $ takeWhile (/= ',') veloccityStr :: Int
        dy = read $ drop 1 $ dropWhile (/= ',') veloccityStr :: Int

--     { pos=trace (show $ warpInBounds gridWidth gridHeight $ apply velocity pos) $ warpInBounds gridWidth gridHeight $ trace (show $ apply velocity pos) apply velocity pos

step gridWidth gridHeight Robot{..} = Robot
    { pos= warpInBounds gridWidth gridHeight $ apply velocity pos
    , velocity=velocity}

countQuadrants :: Int -> Int -> [Robot] -> Int
countQuadrants gridWidth gridHeight robots = trace (show (xCut, yCut, q1, q2, q3, q4)) $ q1 * q2 * q3 * q4
    where
        xCut = gridWidth `div` 2
        yCut = gridHeight `div` 2
        q1 = length $ filter (\r -> x (pos r) < xCut && y (pos r) < yCut) robots
        q2 = length $ filter (\r -> x (pos r) > xCut && y (pos r) < yCut) robots
        q3 = length $ filter (\r -> x (pos r) > xCut && y (pos r) > yCut) robots
        q4 = length $ filter (\r -> x (pos r) < xCut && y (pos r) > yCut) robots


part2 :: [Char] -> Maybe Int
part2 inputStr = Nothing
