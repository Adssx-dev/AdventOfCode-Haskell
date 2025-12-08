module Solutions.Y2025.Day7
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.Map2D (parseInputToMap2d)
import qualified Data.Map as M
import qualified Data.Set as Set
import Utils.Geometry2D

-- Parsing
data Cell = Source | Splitter | Empty deriving (Eq, Show)

parseCell 'S' = Source
parseCell '^' = Splitter
parseCell '.' = Empty

-- Simulation

-- Simulate the whole process (part 1 only)
-- The result is the end position of the rays + the number of divisions (= number of splitters hit)
simulation :: [Point] -> Set.Set Point -> ([Point], Int)
simulation beams splitters = until (\b ->  lastSplitterYPos < y (head $ fst b)) step (beams, 0)
    where
        lastSplitterYPos = Set.findMax $ Set.map y splitters
        -- Step the simulation by 1 unit of time (merge all identical rays & count the total number of splits happening)
        step b = foldl (\(op, oc) (np, nc) -> (nub op ++ np, oc + nc )) ([], snd b) $ map (advanceBeam splitters) (fst b)

-- Advance one beam of 1 position, possibliy splitting it
-- The output is the list of new beams (1 elem if it is not splitted, 2 elems if splitted)
-- and the number of splits (0 or 1)
advanceBeam :: Set.Set Point -> Point -> ([Point], Int)
advanceBeam splitters beam = if Set.member beam splitters
    then ([leftBeamPos, rightBeamPos], 1)
    else ([newBeamPos], 0)
    where
        newBeamPos = Point{x=x beam, y=y beam + 1}
        leftBeamPos = Point{x=x newBeamPos - 1, y=y newBeamPos}
        rightBeamPos = Point{x=x newBeamPos + 1, y=y newBeamPos}

-- Similar to part 1 but also counts the number of similar rays travelling together along with the points
-- This serves as a kind of memoization
simulationPt2 :: [(Point, Int)] -> Set.Set Point -> ([(Point, Int)], Int)
simulationPt2 beams splitters = until (\b ->  lastSplitterYPos < y (fst $ head $ fst b)) step (beams, 0)
    where
        lastSplitterYPos = Set.findMax $ Set.map y splitters
        step b = foldl (\(op, oc) (np, nc) -> (mergeBeamsPt2 op ++ np, oc + nc )) ([], snd b) $ map (advanceBeamPt2 splitters) (fst b)

-- Merge beams that are on the same spot by grouping them and summing their associated value
mergeBeamsPt2 :: [(Point, Int)] -> [(Point, Int)]
mergeBeamsPt2 beams = map (\e -> (fst $ head e, sum $ map snd e)) $ groupBy (\a b -> fst a == fst b) beams

-- Similar to part 1 but the number of splits is set to the number of rays travelling together
advanceBeamPt2 :: Set.Set Point -> (Point, Int) -> ([(Point, Int)], Int)
advanceBeamPt2 splitters (beam, beamCount) = if Set.member beam splitters
    then ([(leftBeamPos, beamCount), (rightBeamPos, beamCount)], beamCount)
    else ([(newBeamPos, beamCount)], 0)
    where
        newBeamPos = Point{x=x beam, y=y beam + 1}
        leftBeamPos = Point{x=x newBeamPos - 1, y=y newBeamPos}
        rightBeamPos = Point{x=x newBeamPos + 1, y=y newBeamPos}

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ snd $ simulation [initialPoint] splittersPos
    where
        parsed = parseInputToMap2d parseCell inputStr
        sourcePos = fst $ head $ M.toList $ M.filter (== Source) parsed
        initialPoint = Point{x=x sourcePos, y= y sourcePos + 1}
        splittersPos = Set.fromList $ map fst $ M.toList $ M.filter (== Splitter) parsed


-- Why a +1 ? I have no idea but it works both on example data and on actual input
part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ 1 + snd (simulationPt2 [(initialPoint, 1)] splittersPos)
    where
        parsed = parseInputToMap2d parseCell inputStr
        sourcePos = fst $ head $ M.toList $ M.filter (== Source) parsed
        initialPoint = Point{x=x sourcePos, y= y sourcePos + 1}
        splittersPos = Set.fromList $ map fst $ M.toList $ M.filter (== Splitter) parsed
