import System.IO
import Debug.Trace
import Data.List
import System.Directory (Permissions(executable))
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe



main = do
    handle <- openFile "data/2020/Day9.txt" ReadMode
    contents <- hGetContents handle

    let allNumbers = map read (lines contents) :: [Int]
    let (preamble, numbers) = splitAt 25 allNumbers
    -- Part 1
    -- function expects preamble to be reversed !
    let invalidNumber = validateList numbers (reverse preamble)
    print invalidNumber

    -- Part 2

    print $  part2 allNumbers invalidNumber

    hClose handle

part2 :: [Int] -> Int -> Int
part2 list target = minimum resultList + maximum resultList
    where
        result = testAllWindowsToGetTarget list 2 target
        resultList = snd $ head result

testAllWindowsToGetTarget list windowSize target = result
    where 
        result = case findContiguousThatSumToTarget list windowSize target of
            [] -> testAllWindowsToGetTarget list (windowSize + 1) target
            x -> x

findContiguousThatSumToTarget list windowSize target = [(sum window, window) | window <- windows, sum window == target]--zip (map sum windows) windows
    where
        windows = movingWindow list windowSize

movingWindow list width = if width >= length list then
            []
        else
            take width list : movingWindow (tail list) width

validateList :: [Int] -> [Int] -> Int
validateList (currentElem:remaining) lastDigits = if found then validateList remaining (currentElem:take 25 lastDigits) else currentElem
    where
        possibleCombinations = [x + y | x <- lastDigits, y <- lastDigits, x /= y]
        found = currentElem `elem` possibleCombinations
