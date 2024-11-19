module Solutions.Y2023.Day5
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char


data MapLine = MapLine {sourceStart :: Int, sourceEnd :: Int, delta :: Int} deriving (Show, Eq, Ord)

part1 :: [Char] -> Maybe Int
part1 inputStr = do
    let sections = splitStr "\n\n" inputStr
    let seedsStr = head sections
    let seeds = map read (tail $ words seedsStr) :: [Int]
    let mapsStr = map lines $ tail sections

    let maps = map parseMap mapsStr
    let allSeedsFinalValues = map (traverseMaps maps) seeds
    Just $ minimum allSeedsFinalValues


part2 :: [Char] -> Maybe Int
part2 inputStr = do
    let sections = splitStr "\n\n" inputStr
    let seedsStr = head sections
    let seeds = map read (tail $ words seedsStr) :: [Int]
    let mapsStr = map lines $ tail sections

    let maps = map parseMap mapsStr
    let allSeedsFinalValues = map (traverseMaps maps) seeds

    Just $ minimum $ solvePart2 seeds maps

solvePart2 [] _ = [999999999]
solvePart2 (start:count:xs) maps = minimum (map (traverseMaps maps) [start..(start + count)]) : solvePart2 xs maps

traverseMaps :: [[MapLine]] -> Int -> Int
traverseMaps nextMaps value
  = foldl (flip convertByMap) value nextMaps

-- traverseMaps :: [MapLine] -> Int -> Int
-- traverseMaps nextMaps value
--   = foldl (flip convertByMap) value nextMaps

parseMapLine line = MapLine { sourceStart = source, sourceEnd = source + size - 1, delta = destination - source}
    where
        [destination, source, size] = map read $ words line :: [Int]

parseMap = map parseMapLine . tail

convertByMap :: [MapLine] -> Int -> Int
convertByMap [] value = value
convertByMap (mapLine:remaining) value
    | value >= sourceStart mapLine && value <= sourceEnd mapLine = value + delta mapLine
    | otherwise = convertByMap remaining value

-- Split a list on a given predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'

splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _   []  subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise            = split' sub (tail str) (head str:subacc) acc