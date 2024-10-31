import System.IO
import Debug.Trace
import Data.List
import Data.Char

data Coordinates = Coordinates {row :: Int, col :: Int} deriving (Show, Eq, Ord)

main = do
    handle <- openFile "data/2016/Day1.txt" ReadMode
    contents <- hGetContents handle

    print $ part1 contents
    --print $ part2  contents

    hClose handle

part1 :: [Char] -> Int
part1 inputStr = abs(row position) + abs(col position)
    where
        position = executeAll (words inputStr) Coordinates{row=0,col=0} Coordinates{row=0, col=1}

executeAll [] position _ = position
executeAll (c:commands) position direction =  executeAll commands newPosition newDirection
    where
        (newPosition, newDirection) = executeCommand c position direction

-- part2 :: [Char] -> Int
-- part2 inputStr = 1 + length (takeWhile (>=0) $ scanl1 (+) $ map convertParenthesis inputStr)

executeCommand (dirMod:length) coords currentDirection = (iterate (addCoordinates newDirection) coords !! n, newDirection)
    where
        n = read (filter isDigit length) :: Int -- parse count with maybe trailing coma
        newDirection = if dirMod == 'L' then turnLeft currentDirection else turnRight currentDirection

-- Coordinates are used as vectors in 2-D space
turnLeft :: Coordinates -> Coordinates
turnLeft Coordinates{row=r, col=c} = Coordinates{row= -c, col=r}

turnRight :: Coordinates -> Coordinates
turnRight Coordinates{row=r, col=c} = Coordinates{row= c, col= -r}

addCoordinates :: Coordinates -> Coordinates -> Coordinates
addCoordinates Coordinates{row=row1, col=col1} Coordinates{row=row2, col=col2} = Coordinates{row = row1 + row2, col = col1 + col2}

