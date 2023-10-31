import System.IO
import Debug.Trace
import Data.List


main = do
    handle <- openFile "data/2020/Day6.txt" ReadMode
    contents <- hGetContents handle

    print $ sum $ numPerGroupPt1 contents
    print $ sum $ numPerGroupPt2 contents

    hClose handle


numPerGroupPt1 contents =  map length idsPerGroup
    where
        groups = splitOn (=="") $ lines contents
        idsPerGroup = map (nub . concat) groups


numPerGroupPt2 contents = map checkGroupAnswers groupsAndLength
    where
        groups = splitOn (=="") $ lines contents -- List of list of anwsers per group : [["abc"],["a","b","c"],["ab","ac"],["a","a","a","a"],["b"]]
        answersPerGroup = map (group . sort . concat) groups -- At that point :  [["a","b","c"],["a","b","c"],["aa","b","c"],["aaaa"],["b"]]
        groupLength = map length groups
        groupsAndLength = zip groupLength answersPerGroup   -- [(1,["a","b","c"]),(3,["a","b","c"]),(2,["aa","b","c"]),(4,["aaaa"]),(1,["b"])]

-- Takes a group with all answers like (2,["aa","b","c"]),
-- And check for each answer if everyone in the group gave it
-- (e.g if there are 2 "a" and 2 people in the group we can assume everyone answered it)
-- Only works if duplicated answers are removed for each individual
checkGroupAnswers groupAnswers = length $ filter id $ map (\x -> length x == fst groupAnswers) (snd groupAnswers)

-- Split a list on a given predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'
