module Utils.List
where

-- Swap two elements from a list based on their indexes
swapTwo f s xs = zipWith (\x y ->
    if x == f then xs !! s
    else if x == s then xs !! f
    else y) [0..] xs