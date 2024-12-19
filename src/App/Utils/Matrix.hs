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
    { a :: Int
    , b :: Int
    , c :: Int
    , d :: Int
    } deriving (Eq, Show, Ord)


mul :: Matrix2 -> Vector -> Vector
mul Matrix2{..} Vector{..} = Vector{dx = a * dx + b *  dy, dy = c * dx + d *  dy}

solveLinearEquation :: Matrix2 -> Vector -> Maybe Vector
solveLinearEquation mat target = if mul mat candidate == target || det == 0
        then Just candidate
        else Nothing
    where
        det = determinant mat
        candidate = Vector
            {dx= (d mat * dx target - b mat * dy target) `div` det
            ,dy= (-c mat * dx target + a mat * dy target) `div` det}

determinant Matrix2{..} = a * d - b * c

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