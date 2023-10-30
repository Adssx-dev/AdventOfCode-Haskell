import System.IO
import Debug.Trace
import Data.List


main = do
    handle <- openFile "data/2020/Day5.txt" ReadMode
    contents <- hGetContents handle
    let passes = lines contents
    let passesIds = sort $ map (parseBoardingPass 0) passes
    print $ maximum passesIds

    print $ findSeat passesIds [1..1024]

    hClose handle

findSeat passesIds possibleSeats = case result of
    Nothing -> 0
    Just value -> candidates !! value
    where
        candidates = map (+1) $ elemIndices True $ map (`notElem` passesIds) possibleSeats
        result = trace (show candidates) elemIndex True $ map (\x -> (x - 1) `elem` passesIds && (x + 1) `elem` passesIds) candidates

parseBoardingPass :: Int -> [Char] -> Int
parseBoardingPass value [] = value
parseBoardingPass value pass = parseBoardingPass (value * 2 + currentValue) (tail pass)
    where currentValue = case head pass of
            'B' -> 1
            'R' -> 1
            _ -> 0
