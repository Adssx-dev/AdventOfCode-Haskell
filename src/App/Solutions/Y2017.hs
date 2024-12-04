module Solutions.Y2017
where

import Data.Map as Map
import Solutions.Y2017.Day1
import Solutions.Y2017.Day2
import Solutions.Y2017.Day3
import Solutions.Y2017.Day4
import Solutions.Y2017.Day5
import Solutions.Y2017.Day6
import Solutions.Y2017.Day7
import Solutions.Y2017.Day8
import Solutions.Y2017.Day9
import Solutions.Y2017.Day10
import Solutions.Y2017.Day11
import Solutions.Y2017.Day12
import Solutions.Y2017.Day13
import Solutions.Y2017.Day14
import Solutions.Y2017.Day15
import Solutions.Y2017.Day16
import Solutions.Y2017.Day17
import Solutions.Y2017.Day18
import Solutions.Y2017.Day19
import Solutions.Y2017.Day20
import Solutions.Y2017.Day21
import Solutions.Y2017.Day22
import Solutions.Y2017.Day23
import Solutions.Y2017.Day24
import Solutions.Y2017.Day25

dayMap :: Map Int ([Char] -> Maybe Int, [Char] -> Maybe Int)
dayMap = Map.fromList . zip [1..] $
    [ (Solutions.Y2017.Day1.part1, Solutions.Y2017.Day1.part2)
    , (Solutions.Y2017.Day2.part1, Solutions.Y2017.Day2.part2)
    , (Solutions.Y2017.Day3.part1, Solutions.Y2017.Day3.part2)
    , (Solutions.Y2017.Day4.part1, Solutions.Y2017.Day4.part2)
    , (Solutions.Y2017.Day5.part1, Solutions.Y2017.Day5.part2)
    , (Solutions.Y2017.Day6.part1, Solutions.Y2017.Day6.part2)
    , (Solutions.Y2017.Day7.part1, Solutions.Y2017.Day7.part2)
    , (Solutions.Y2017.Day8.part1, Solutions.Y2017.Day8.part2)
    , (Solutions.Y2017.Day9.part1, Solutions.Y2017.Day9.part2)
    , (Solutions.Y2017.Day10.part1, Solutions.Y2017.Day10.part2)
    , (Solutions.Y2017.Day11.part1, Solutions.Y2017.Day11.part2)
    , (Solutions.Y2017.Day12.part1, Solutions.Y2017.Day12.part2)
    , (Solutions.Y2017.Day13.part1, Solutions.Y2017.Day13.part2)
    , (Solutions.Y2017.Day14.part1, Solutions.Y2017.Day14.part2)
    , (Solutions.Y2017.Day15.part1, Solutions.Y2017.Day15.part2)
    , (Solutions.Y2017.Day16.part1, Solutions.Y2017.Day16.part2)
    , (Solutions.Y2017.Day17.part1, Solutions.Y2017.Day17.part2)
    , (Solutions.Y2017.Day18.part1, Solutions.Y2017.Day18.part2)
    , (Solutions.Y2017.Day19.part1, Solutions.Y2017.Day19.part2)
    , (Solutions.Y2017.Day20.part1, Solutions.Y2017.Day20.part2)
    , (Solutions.Y2017.Day21.part1, Solutions.Y2017.Day21.part2)
    , (Solutions.Y2017.Day22.part1, Solutions.Y2017.Day22.part2)
    , (Solutions.Y2017.Day23.part1, Solutions.Y2017.Day23.part2)
    , (Solutions.Y2017.Day24.part1, Solutions.Y2017.Day24.part2)
    , (Solutions.Y2017.Day25.part1, Solutions.Y2017.Day25.part2)
    ]