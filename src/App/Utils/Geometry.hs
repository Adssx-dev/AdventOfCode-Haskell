{-# LANGUAGE RecordWildCards #-}
module Utils.Geometry
where

data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Eq, Show, Ord)


data Vector = Vector
    { dx :: Int
    , dy :: Int
    } deriving (Eq, Show, Ord)

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

apply :: Vector -> Point -> Point
apply Vector{ .. } Point{ .. } = Point{x=x+dx, y=y+dy}

turnRight Vector{ .. } = Vector {dx = -1 * dy, dy =  dx}

turnLeft Vector{ .. } = Vector {dx =  dy, dy = -1 * dx}