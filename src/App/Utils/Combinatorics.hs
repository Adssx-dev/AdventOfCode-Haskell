module Utils.Combinatorics
where

-- Generate the list of all combinations of elements appearing or not 
-- Example: [1, 2, 3, 4] => [[1,2,3,4],[1,2,3],[1,2,4],[1,2],[1,3,4],[1,3],[1,4],[1],[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
generateAllCombinations [] = [[]]
generateAllCombinations (x:xs) = map (x:) subLists ++ subLists
    where
        subLists = generateAllCombinations xs

