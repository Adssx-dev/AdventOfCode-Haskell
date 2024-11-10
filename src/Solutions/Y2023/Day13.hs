import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char


main = do
    handle <- openFile "data/2023/Day13.txt" ReadMode
    contents <- hGetContents handle

    let patterns = splitOn (== "") $ lines contents

    print $ part1 patterns

    hClose handle

part1 patterns = sum $ map findMirrorPosition patterns


findMirrorPosition pattern =  100 * fromMaybe 0 indexY + fromMaybe 0 indexX
    where
        splitsY = generateAllPossibleSplits pattern
        splitsX = generateAllPossibleSplits $ transpose pattern
        indexY = (+1) <$> findIndex equalWithinLimits splitsY
        indexX = (+1) <$> findIndex equalWithinLimits splitsX

generateAllPossibleSplits lst = sortBy (compare `on` length) $ zip
    (map reverse $ drop 1 $ init $ inits lst)
    (drop 1 $ init $ tails lst)

equalWithinLimits :: Eq a => ([a], [a]) -> Bool
equalWithinLimits (       _,       []) = True
equalWithinLimits (      [],        _) = True
equalWithinLimits (x1:xs1, x2:xs2) = (x1 == x2) && equalWithinLimits (xs1, xs2)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'