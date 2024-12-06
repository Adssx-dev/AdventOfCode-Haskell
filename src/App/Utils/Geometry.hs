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


apply :: Vector -> Point -> Point
apply Vector{ .. } Point{ .. } = Point{x=x+dx, y=y+dy}

turnRight Vector{ .. } = Vector {dx = -1 * dy, dy =  dx}

turnLeft Vector{ .. } = Vector {dx =  dy, dy = -1 * dx}