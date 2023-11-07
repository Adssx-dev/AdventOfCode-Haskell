import System.IO
import Debug.Trace
import Data.List

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)


main = do
    handle <- openFile "data/2020/Day7.txt" ReadMode
    contents <- hGetContents handle

    let target = "shinygold"

    let bagList = map parseLine $ lines contents

    let reverseBagList = Map.fromList $ reverseChildrenParents bagList
    --print reverseBagList
    let results = nub $ searchInTree reverseBagList target
    print $ results
    print $ length results - 1
    --print $ getChildren bagMap "wavywhite"


    hClose handle


searchInTree :: Map.Map String [String] -> String -> [String]
searchInTree _ [] = []
searchInTree tree bagColor = bagColor : concatMap (searchInTree tree) newElems
    where
        newElems = fromMaybe [] $ Map.lookup bagColor tree

reverseChildrenParents :: (Ord a1, Foldable t) => t (b, [(a2, a1)]) -> [(a1, [b])]
reverseChildrenParents bagList = map (\x ->  (fst $ head x, map snd x))  grouped
    where
        grouped = groupBy groupingFunction $ sortBy sortingFunction $ concatMap reverseBag bagList
        sortingFunction x y = compare (fst x)  (fst y)
        groupingFunction x y = fst x == fst y

reverseBag bag = map (\x -> (snd x, fst bag)) $ snd bag

getChildren :: Map.Map String [(Int, String)] -> String -> [String]
getChildren bagMap bag = map snd children
    where
        Just children = Map.lookup bag bagMap


parseLine :: [Char] -> ([Char], [(Int, [Char])])
parseLine line = (adjective ++ color, parseBagContent next)
    where
        (adjective:color:_:_:next) = words line


parseBagContent ["no", "other", "bags."] = []
parseBagContent [number, adjective, color, _] = [(read number :: Int, adjective ++ color)]
parseBagContent (number:adjective:color:_:next) = (read number :: Int, adjective ++ color) : parseBagContent next



-- Split a list on a given predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'
