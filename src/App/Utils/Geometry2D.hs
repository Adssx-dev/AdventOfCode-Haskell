{-# LANGUAGE RecordWildCards #-}
module Utils.Geometry2D
where

import Utils.Math
import Utils.List (splitOn)

data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Eq, Show, Ord)


data Vector = Vector
    { dx :: Int
    , dy :: Int
    } deriving (Eq, Show, Ord)

parsePoint2 :: Char -> String -> Point
parsePoint2 separator str = Point{x=read vx, y = read vy}
    where
        [vx, vy] = splitOn (==separator) str

-- Get the area of the rectangle defined by both corners p1 and p2
rectangleArea :: Point -> Point -> Int
rectangleArea p1 p2 = (x p1 - x p2 + 1) * (y p1 - y p2 + 1)

getTranslationVector :: Point -> Point -> Vector
getTranslationVector Point{x=x1, y=y1} Point{x=x2, y=y2} = Vector{dx = x2 - x1, dy = y2 - y1}

reverseVector :: Vector -> Vector
reverseVector Vector{..} = Vector{dx = -1 * dx, dy = -1 * dy}

getNeighbors4 Point{ .. } = [Point{x=x-1, y=y}, Point{x=x, y=y-1}, Point{x=x+1, y=y}, Point{x=x, y=y+1}]

getNeighbors2UD Point{ .. } = [Point{x=x, y=y-1}, Point{x=x, y=y+1}]

getNeighbors2LR Point{ .. } = [Point{x=x-1, y=y},  Point{x=x+1, y=y}]

getNeighbors8 Point{ .. } = [
    Point{x=x-1, y=y},
    Point{x=x-1, y=y-1},
    Point{x=x, y=y-1},
    Point{x=x+1, y=y-1},
    Point{x=x+1, y=y},
    Point{x=x+1, y=y+1},
    Point{x=x, y=y+1},
    Point{x=x-1, y=y+1}]

warpInBounds :: Int -> Int -> Point -> Point
warpInBounds gridWidth gridHeight Point{..} = Point{x=x `posMod` gridWidth, y = y `posMod` gridHeight}

apply :: Vector -> Point -> Point
apply Vector{ .. } Point{ .. } = Point{x=x+dx, y=y+dy}

turnRight Vector{ .. } = Vector {dx = -1 * dy, dy =  dx}

turnLeft Vector{ .. } = Vector {dx =  dy, dy = -1 * dx}

-- the origin is at the top left corner (Y increases when going down, x when goin right)
vectorFromAscii :: Char -> Vector
vectorFromAscii '^' = Vector{dx =  0, dy = -1}
vectorFromAscii '>' = Vector{dx =  1, dy =  0}
vectorFromAscii 'v' = Vector{dx =  0, dy =  1}
vectorFromAscii '<' = Vector{dx = -1, dy =  0}
vectorFromAscii c = error $ "Forbidden character in ASCII to vector : " ++  show c


collision1d :: Int -> Int -> Int -> Int -> Bool
collision1d a1 a2 b1 b2 = max a1 b1 <= (min a2 b2)