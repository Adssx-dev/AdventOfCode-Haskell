{-# LANGUAGE RecordWildCards #-}
module Utils.Matrix 
where

import Data.List
import qualified Data.Map as Map

import Utils.Geometry


-- Represent a matrix 2x2 with data as 
-- | a b |
-- | c d |
data Matrix2 = Matrix2
    { a :: Double
    , b :: Double
    , c :: Double
    , d :: Double
    } deriving (Eq, Show, Ord)

data Vector2f = Vector2f
    { dx :: Double
    , dy :: Double
    } deriving (Eq, Show, Ord)

mul :: Matrix2 -> Vector2f -> Vector2f
mul Matrix2{..} Vector2f{..} = Vector2f{dx = a * dx + b *  dy, dy = c * dx + d *  dy}

inv :: Matrix2 -> Matrix2
inv Matrix2{..} = Matrix2
    { a =  d * det
    , b = -b * det
    , c = -c * det
    , d =  a * det
    }
    where 
        det = 1 / (a * d - b * c)

-- Get all the diagonals of a matrix perpendicular to principal diagonal
-- Transpose the matrix to get the other diagonals
diagonals = map concat
          . transpose
          . zipWith (\ns xs -> ns ++ map (:[]) xs)
                    (iterate ([]:) [])

toCoordinateMap :: [[a]] -> Map.Map Point a
toCoordinateMap mat = Map.fromList $ toCoordinateList mat

toCoordinateList :: [[a]] -> [(Point, a)]
toCoordinateList mat = concat $ zipWith 
            (\row lst -> map (\(col, value) -> (Point{x=col, y=row}, value)) lst) 
            [0..] -- row coordinates
            (map (zip [0..]) mat) -- col coordinate + value