import System.IO
import Debug.Trace
import Data.List

data Coordinates = Coordinates {row :: Int, col :: Int} deriving (Show, Eq, Ord)

main = do
    handle <- openFile "data/2015/Day3.txt" ReadMode
    contents <- hGetContents handle

    print $ part1 contents
    print $ part2 contents

    hClose handle

part1 :: [Char] -> Int
part1 inputStr = length $ nub $ presentsInDelivery inputStr

part2 :: [Char] -> Int
part2 inputStr = length $ nub (housesSanta1 ++ housesSanta2)
    where
        (santa1, santa2) = splitEvenOdd inputStr
        housesSanta1 = presentsInDelivery santa1
        housesSanta2 = presentsInDelivery santa2

presentsInDelivery inputStr = scanl addCoordinates Coordinates{row=0, col=0} $ map directionToCoordinates $ inputStr

splitEvenOdd lst = (filterList lst odds, filterList lst evens)
    where
        odds  = map ((==1) . (`mod` 2)) [2..]
        evens = map ((==1) . (`mod` 2)) [1..]
        filterList lst filterLst = map fst $ filter snd (zip lst filterLst)

-- Origin is bottom left
directionToCoordinates 'v' = Coordinates {row = -1, col=  0}
directionToCoordinates '^' = Coordinates {row =  1, col=  0}
directionToCoordinates '<' = Coordinates {row =  0, col= -1}
directionToCoordinates '>' = Coordinates {row =  0, col=  1}
directionToCoordinates char = error $ "Unrecognised character " ++ [char]

addCoordinates :: Coordinates -> Coordinates -> Coordinates
addCoordinates Coordinates{row=row1, col=col1} Coordinates{row=row2, col=col2} = Coordinates{row = row1 + row2, col = col1 + col2}

