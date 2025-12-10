module Utils.List
where

import Data.List

-- Swap two elements from a list based on their indexes
swapTwo f s xs = zipWith (\x y ->
    if x == f then xs !! s
    else if x == s then xs !! f
    else y) [0..] xs

pairs :: [a] -> [(a, a)]
pairs list = [(x,y) | (x:ys) <- tails list, y <- ys]

safeHead [] = Nothing
safeHead (x:xs) = Just x

-- Split a list on a given predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'

-- Remove nth element of the list, indes starts at 0
removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt 0 (x:xs) = xs
removeAt n (x:xs) = x : removeAt (n-1) xs

-- Generate pairs of elements with a given function
-- Works as a zip function in 2D only taking the top triangle matrix
-- This is to use in case when f(a, b) == f(b, a) to avoid duplication
--
-- Example with the list [a, b, c, d] (X marks pairs that are made)
-- |   | a | b | c | d |
-- | a |   | X | X | X |
-- | b |       | X | X |
-- | c |           | X |
-- | d |               |
--
-- The generated pairs are [f(a, b), f(a, c), f(a, d), f(b, c), f(b, d), f(c, d)] 
generatePairsCommutative :: (a -> a -> b) -> [a] -> [b]
generatePairsCommutative fcn points = concat $
    zipWith (map . fcn)
        (init points)
        (tails points)

-- Convert a list of indexes to the list of booleans associated (Zero-indexed)
-- Ex: [1, 2, 3] => [False, True, True, True]
-- Stops at the highest element
indicesToBooleanList [] _ = []
indicesToBooleanList (ind:indices) currentIndex = if currentIndex == ind
    then True: indicesToBooleanList indices (currentIndex + 1) 
    else False:indicesToBooleanList (ind:indices) (currentIndex + 1)


removeFirstAndLast :: [a] -> [a]
removeFirstAndLast = init . tail