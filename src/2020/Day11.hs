import System.IO
import Debug.Trace
import Data.List
import Data.Maybe

data Status = Occupied | Empty deriving (Show, Eq)
data Coordinates = Coordinates {row :: Int, column :: Int} deriving (Show, Eq, Ord)
data Seat = Seat {status :: Status, coordinates :: Coordinates} deriving (Show, Eq)

main = do
    handle <- openFile "data/2020/Day11.txt" ReadMode
    contents <- hGetContents handle

    let coords = linesToCoordinates 0 $ lines contents
    let seats = map (Seat Empty . uncurry Coordinates) coords

    -- print $ seats == seats
    -- print $ calculateNextRound seats
    -- print $ calculateNextRound $ calculateNextRound seats
    print $ findEquilibrumRound seats 1

    hClose handle

findEquilibrumRound :: [Seat] -> Int -> Int
findEquilibrumRound seats roundNumber = trace (show roundNumber) $ if newRoundSeats == seats
    then
        length $ filter (\x -> status x == Occupied) seats
    else
        findEquilibrumRound newRoundSeats (roundNumber + 1)
    where
        newRoundSeats = calculateNextRound seats


calculateNextRound :: [Seat] -> [Seat]
calculateNextRound seats = nextRound
    where
        seatsCoordinates = map coordinates seats
        occupiedSeatsInfluence = concatMap (surroundingCoordinates . coordinates) $ filter (\x -> status x == Occupied) seats
        occupiedNeighbors = map (\x -> (head x, length x)) $ group $ sort $ filter (`elem` seatsCoordinates) occupiedSeatsInfluence
        nextRound = map (caculateNewStatus occupiedNeighbors) seats


caculateNewStatus ::  [(Coordinates, Int)] -> Seat -> Seat
caculateNewStatus occupiedNeighbors seat =
    case (status seat, noNeighbors, moreThan4Neighbors) of
        (Empty, True, _) -> Seat Occupied (coordinates seat)
        (Occupied, _, True) -> Seat Empty (coordinates seat)
        (status, _, _)-> Seat status (coordinates seat)
    where
        neighborCount = snd $ fromMaybe (Coordinates 0 0, 0) $ find (\x -> coordinates seat == fst x) occupiedNeighbors
        noNeighbors = neighborCount == 0
        moreThan4Neighbors = neighborCount >= 4


linesToCoordinates :: Int -> [[Char]] -> [(Int, Int)]
linesToCoordinates  _ [] = []
linesToCoordinates lineNumber (currentLine:remainingLines) = coordinates ++ linesToCoordinates (lineNumber + 1) remainingLines
    where
        indicesInLine = elemIndices 'L' currentLine
        coordinates = map (lineNumber,) indicesInLine -- same a lamda (\x -> (lineNumber, x))


surroundingCoordinates (Coordinates  {row, column})= [Coordinates {row=rowNew, column=colNew} | rowNew <- [row-1..row+1], colNew<-[column-1..column+1], not ( row== rowNew && column ==colNew)]

