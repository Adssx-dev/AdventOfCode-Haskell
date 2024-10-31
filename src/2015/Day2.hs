import System.IO
import Debug.Trace
import Data.List

data Status = StatusDone [Int] | StatusRunning [Int]

main = do
    handle <- openFile "data/2015/Day2.txt" ReadMode
    contents <- hGetContents handle

    print $ part1 contents
    --print $ part2  contents

    hClose handle

part1 :: [Char] -> Int
part1 inputStr = sum $ map (computeArea . parsePresents) $ lines inputStr


--part2 :: [Char] -> Int
--part2 inputStr = 1 + length (takeWhile (>=0) $ scanl1 (+) $ map convertParenthesis inputStr)

computeArea :: (Int, Int, Int) -> Int
computeArea (l, w, h) = minimum sides + 2 * sum sides 
    where
        sides = [l * w, w * h, h * l]

parsePresents :: [Char] -> (Int, Int, Int)
parsePresents sizeStr = (read (splitted !! 0) :: Int, read (splitted !! 1) :: Int, read (splitted !! 2) ::Int)
    where
        splitted = splitOn (=='x') sizeStr

-- Split a list on a given predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'