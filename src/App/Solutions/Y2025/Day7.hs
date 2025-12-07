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
import Utils.Geometry

-- Parsing
data Cell = Source | Splitter | Empty deriving (Eq, Show)

parseCell 'S' = Source
parseCell '^' = Splitter
parseCell '.' = Empty

-- Simulation

simulation :: [Point] -> Set.Set Point -> ([Point], Int)
--simulation beams splitters = until (\b -> trace (show (lastSplitterYPos, b)) lastSplitterYPos < y (head $ fst b)) step (beams, 0)
simulation beams splitters = until (\b ->  lastSplitterYPos < y (head $ fst b)) step (beams, 0)
    where
        lastSplitterYPos = Set.findMax $ Set.map y splitters
        step b = foldl (\(op, oc) (np, nc) -> (nub op ++ np, oc + nc )) ([], snd b) $ map (advanceBeam splitters) (fst b)


advanceBeam :: Set.Set Point -> Point -> ([Point], Int)
advanceBeam splitters beam = if Set.member beam splitters
    then ([leftBeamPos, rightBeamPos], 1)
    else ([newBeamPos], 0)
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



part2 :: [Char] -> Maybe Int
part2 inputStr = Nothing
