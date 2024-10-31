import System.IO
import Debug.Trace
import Data.List

data Status = StatusDone [Int] | StatusRunning [Int]

main = do
    handle <- openFile "data/2015/Day1.txt" ReadMode
    contents <- hGetContents handle

    print $ part1 contents
    print $ part2  contents

    hClose handle

part1 :: [Char] -> Int
part1 inputStr = sum $ map convertParenthesis inputStr


part2 :: [Char] -> Int
part2 inputStr = 1 + length (takeWhile (>=0) $ scanl1 (+) $ map convertParenthesis inputStr)


convertParenthesis :: Char -> Int
convertParenthesis '('  =  1
convertParenthesis ')'  = -1
convertParenthesis char = error $ "Invalid character in sequence" ++ [char]

