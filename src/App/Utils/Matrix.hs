module Utils.Matrix 
where

import Data.List
import qualified Data.Map as Map

import Utils.Geometry

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