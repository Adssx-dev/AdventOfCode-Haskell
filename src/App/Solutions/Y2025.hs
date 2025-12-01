module Solutions.Y2025
where

import Data.Map as Map
import Solutions.Y2025.Day1
import Solutions.Y2025.Day2
import Solutions.Y2025.Day3
import Solutions.Y2025.Day4
import Solutions.Y2025.Day5
import Solutions.Y2025.Day6
import Solutions.Y2025.Day7
import Solutions.Y2025.Day8
import Solutions.Y2025.Day9
import Solutions.Y2025.Day10
import Solutions.Y2025.Day11
import Solutions.Y2025.Day12

dayMap :: Map Int ([Char] -> Maybe Int, [Char] -> Maybe Int)
dayMap = Map.fromList . zip [1..] $
    [ (Solutions.Y2025.Day1.part1, Solutions.Y2025.Day1.part2)
    , (Solutions.Y2025.Day2.part1, Solutions.Y2025.Day2.part2)
    , (Solutions.Y2025.Day3.part1, Solutions.Y2025.Day3.part2)
    , (Solutions.Y2025.Day4.part1, Solutions.Y2025.Day4.part2)
    , (Solutions.Y2025.Day5.part1, Solutions.Y2025.Day5.part2)
    , (Solutions.Y2025.Day6.part1, Solutions.Y2025.Day6.part2)
    , (Solutions.Y2025.Day7.part1, Solutions.Y2025.Day7.part2)
    , (Solutions.Y2025.Day8.part1, Solutions.Y2025.Day8.part2)
    , (Solutions.Y2025.Day9.part1, Solutions.Y2025.Day9.part2)
    , (Solutions.Y2025.Day10.part1, Solutions.Y2025.Day10.part2)
    , (Solutions.Y2025.Day11.part1, Solutions.Y2025.Day11.part2)
    , (Solutions.Y2025.Day12.part1, Solutions.Y2025.Day12.part2)
    ]