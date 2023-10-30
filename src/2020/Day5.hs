import System.IO
import Debug.Trace
import Data.List


main = do
    handle <- openFile "data/2020/Day5.txt" ReadMode
    contents <- hGetContents handle
    let passes = lines contents
    print $ maximum $ map (parseBoardingPass 0) passes
    hClose handle

parseBoardingPass :: Int -> [Char] -> Int
parseBoardingPass value [] = value
parseBoardingPass value pass = parseBoardingPass (value * 2 + currentValue) (tail pass)
    where currentValue = case head pass of
            'B' -> 1
            'R' -> 1
            _ -> 0
        