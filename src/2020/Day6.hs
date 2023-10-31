import System.IO
import Debug.Trace
import Data.List


main = do
    handle <- openFile "data/2020/Day6.txt" ReadMode
    contents <- hGetContents handle

    print $ sum $ numPerGroup contents

    hClose handle


numPerGroup contents =  map length idsPerGroup
    where
        groups = splitOn (=="") $ lines contents
        idsPerGroup = map (nub . concat) groups

-- Split a list on a given predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'
