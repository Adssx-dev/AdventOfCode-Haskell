import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char


data MapLine = MapLine {sourceStart :: Int, sourceEnd :: Int, delta :: Int} deriving (Show, Eq, Ord)



main = do
    handle <- openFile "data/2023/Day5.txt" ReadMode
    contents <- hGetContents handle

    let sections = splitStr "\n\n" contents
    let seedsStr = head sections
    let seeds = map read (tail $ words seedsStr) :: [Int]
    let mapsStr = map lines $ tail sections

    let maps = map parseMap mapsStr
    let allSeedsFinalValues = map (traverseMaps maps) seeds
    print $ minimum allSeedsFinalValues

    print $ minimum $ part2 seeds maps


    hClose handle

part2 [] _ = [999999999]
part2 (start:count:xs) maps = minimum (map (traverseMaps maps) [start..(start + count)]) : part2 xs maps

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