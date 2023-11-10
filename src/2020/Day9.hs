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
    print $ validateList numbers (reverse preamble)



    hClose handle

validateList :: [Int] -> [Int] -> Int
validateList (currentElem:remaining) lastDigits = if found then validateList remaining (currentElem:take 25 lastDigits) else currentElem
    where
        possibleCombinations = [x + y | x <- lastDigits, y <- lastDigits, x /= y]
        found = currentElem `elem` possibleCombinations
