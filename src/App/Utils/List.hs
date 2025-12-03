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