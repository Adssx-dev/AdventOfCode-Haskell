module Solutions.Y2020.Day7
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)


part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ length results - 1
    where
        target = "shinygold"
        bagList = map parseLine $ lines inputStr
        bagMap = Map.fromList bagList
        reverseBagList = Map.fromList $ reverseChildrenParents bagList
        results = nub $ searchInTree reverseBagList target



part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ numberOfBagsContained  bagMap target
    where
        target = "shinygold"
        bagList = map parseLine $ lines inputStr
        bagMap = Map.fromList bagList
        reverseBagList = Map.fromList $ reverseChildrenParents bagList
        results = nub $ searchInTree reverseBagList target

numberOfBagsContained :: Map.Map String [(Int, String)] -> String -> Int
numberOfBagsContained bagMap bag =  sum (map (totalNestedChildren bagMap) children) + 1
    where
        children = fromMaybe [] $ Map.lookup bag bagMap


totalNestedChildren bagMap (amount, directChild) = amount * numberOfBagsContained bagMap directChild

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
