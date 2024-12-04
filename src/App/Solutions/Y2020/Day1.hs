module Solutions.Y2020.Day1
( part1
, part2
) where

import System.IO  


part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sums2To2020 (map read $ lines inputStr)


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ sums2To2020 (map read $ lines inputStr)

sums2To2020 :: [Int] -> Int
sums2To2020 lst = head [x * y | x <- lst, y <- lst, x + y  == 2020]

sums3To2020 :: [Int] -> Int
sums3To2020 lst = head [x * y * z | x <- lst, y <- lst, z <- lst, x + y + z  == 2020]
