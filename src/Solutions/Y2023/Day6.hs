import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char




main = do
    handle <- openFile "data/2023/Day6.txt" ReadMode
    contents <- hGetContents handle

    print $ part1 (lines contents)
    print $ part2 (lines contents)



    hClose handle

part2 :: [String] -> Int
part2 [timesStr, distancesStr] = result
    where
        time = read (concat $ tail $ words timesStr) :: Double
        distance = read (concat $ tail $ words distancesStr) :: Double
        result =  numberOfIntegerQradraticSolutions (-1) time (-distance - 0.1)

part1 :: [String] -> Int
part1 [timesStr, distancesStr] = product possibilities
    where
        times = map read (tail $ words timesStr) :: [Int]
        distances = map read (tail $ words distancesStr) :: [Int]
        zipped = zip (map fromIntegral times :: [Double]) (map fromIntegral distances :: [Double])
        possibilities = map (\(t, d) -> numberOfIntegerQradraticSolutions (-1) t (-d - 0.1)) zipped

numberOfIntegerQradraticSolutions :: Double -> Double -> Double -> Int
numberOfIntegerQradraticSolutions a b c =  floor largeRoot - ceiling smallRoot + 1
    where
        delta = b * b - 4 * a * c
        roots = [-(b - sqrt delta)/(2*a), -(b + sqrt delta)/(2*a)]
        smallRoot = minimum roots
        largeRoot = maximum roots
