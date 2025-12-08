{-# LANGUAGE RecordWildCards #-}
module Utils.Geometry3D
where

import Utils.Math
import Utils.List (splitOn)


data Point3 = Point3
    { x :: Int
    , y :: Int
    , z :: Int
    } deriving (Eq, Show, Ord)

data Vector3 = Vector3
    { dx :: Int
    , dy :: Int
    , dz :: Int
    } deriving (Eq, Show, Ord)


apply :: Vector3 -> Point3 -> Point3
apply Vector3{ .. } Point3{ .. } = Point3{x=x+dx, y=y+dy, z=z+dz}

parsePoint3 :: Char -> String -> Point3
parsePoint3 separator str = Point3{x=read vx, y = read vy, z = read vz}
    where
        [vx, vy, vz] = splitOn (==separator) str

squaredLinearDistance :: Point3 -> Point3 -> Int
squaredLinearDistance p1 p2 = diffX * diffX + diffY * diffY + diffZ * diffZ
    where
        diffX = x p1 - x p2
        diffY = y p1 - y p2
        diffZ = z p1 - z p2

linearDistance :: Point3 -> Point3 -> Double
linearDistance p1 p2 = sqrt $ fromIntegral $ squaredLinearDistance p1 p2