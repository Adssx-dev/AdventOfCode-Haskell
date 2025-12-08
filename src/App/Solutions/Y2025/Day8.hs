{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
module Solutions.Y2025.Day8
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.Geometry3D
import qualified Data.Set as Set

-- -------
-- |TYPES|
-- -------

-- Represent the distance between two points
data DistanceEntry = DistanceEntry
    { p1 :: Point3
    , p2 :: Point3
    , distance :: Int
    } deriving(Eq, Show)

instance Ord DistanceEntry where
    compare :: DistanceEntry -> DistanceEntry -> Ordering
    d1 `compare` d2 = distance d1 `compare` distance d2

-- A circuit is a group of points and we want fast access to "is point P in circuit C ?"
data Circuit = Circuit
    { points :: Set.Set Point3
    , connections :: [DistanceEntry]
    } deriving (Eq, Show)

data Status = Status
    { distances :: [DistanceEntry]
    , circuits :: [Circuit]
    , iterationCount :: Int
    } deriving (Eq, Show)

-- --------------
-- |COMPUTATIONS|
-- --------------

computeDistances :: [Point3] -> [DistanceEntry]
computeDistances points = sort $ filter (\de -> distance de /= 0) distances
    where
        --distances = concatMap (\p -> map (\p2-> DistanceEntry{p1=p, p2=p2, distance=squaredLinearDistance p p2}) points) points
        distances = concat $ zipWith (\p1 lst -> map (\p2-> DistanceEntry{p1=p1, p2=p2, distance=squaredLinearDistance p1 p2}) lst) (init points) (tails points)


simulate distances = until (\x ->  iterationCount x == 1000) stepSimulation initialStatus
    where
        initialStatus = Status{distances=distances, circuits=[], iterationCount=0}

stepSimulation :: Status -> Status
stepSimulation Status{..} = Status {distances=remainingDistances, circuits=newCircuit:remainingAfterP2, iterationCount=iterationCount+1}
    where
        (shortestDistance:remainingDistances) = distances
        (setOfP1, remainingAfterP1) = trace (show shortestDistance) partition (Set.member (p1 shortestDistance) . points) circuits
        (setOfP2, remainingAfterP2) = partition (Set.member (p2 shortestDistance) . points) remainingAfterP1
        newCircuit = case (setOfP1, setOfP2) of
            ([], []) -> trace ("cas 1") Circuit{points=Set.fromList [p1 shortestDistance, p2 shortestDistance], connections=[shortestDistance]}
            ([sp1], []) -> trace ("cas 2") Circuit{points=Set.insert (p2 shortestDistance) (points sp1), connections=shortestDistance:connections sp1}
            ([], [sp2]) -> trace ("cas 3") Circuit{points=Set.insert (p1 shortestDistance) (points sp2), connections=shortestDistance:connections sp2}
            ([sp1], [sp2]) -> trace ("cas 4") Circuit{points=Set.union (points sp1) (points sp2), connections=shortestDistance:connections sp1 ++ connections sp2}
            x -> error "Invalid state: multiple sets contain the same point"

calculateResults :: Status -> Int
calculateResults status = product largestCircuits
    where
        largestCircuits = take 3 $ sortBy (flip compare) $ map (length . Set.toList  . points) $ circuits status 

part1 :: [Char] -> Maybe Int
part1 inputStr = trace (show $ sortBy (flip compare) $ map (length . Set.toList  . points) $ circuits simuResults) Just $ calculateResults simuResults
    where
        pts = map (parsePoint3 ',') $ lines inputStr
        distances = computeDistances pts
        simuResults = simulate distances
    

part2 :: [Char] -> Maybe Int
part2 inputStr = Nothing
