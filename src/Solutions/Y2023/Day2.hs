

import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe


main = do
    handle <- openFile "data/2023/Day2.txt" ReadMode
    contents <- hGetContents handle

    let games = map loadGame $ lines contents

    -- part 1
    let possibleGames = filter (isGamePossible [12, 13, 14]) games
    print $ sum $ map fst possibleGames

    print $ sum $ map calculateGamePower games 

    hClose handle

calculateGamePower game = product minCubes
    where
        minCubes = minimumGameCubeNumber game

minimumGameCubeNumber game = map maximum $ transpose rounds
    where 
        rounds = snd game


isGamePossible cubeAmount game = valid
    where
        rounds = snd game
        valid = all (isRoundValid cubeAmount) rounds

isRoundValid cubeAmount roundValues = and $ zipWith (<=) roundValues cubeAmount


-- Example game format : 
-- (GameId, [Triplets of color for erach round])
-- [(1,[[4,0,3],[1,2,6],[0,2,0]]),(2,[[0,2,1],[1,3,4],[0,1,1]]),(3,[[20,8,6],[4,13,5],[1,5,0]]),(4,[[3,1,6],[6,3,0],[14,3,15]]),(5,[[6,3,1],[1,2,2]])]
loadGame line = (read gameIdStr :: Int, rounds)
    where
        [metadata, dat] = splitOn (==':') line
        [_, gameIdStr] = words metadata
        roundsStr = splitOn (==';') dat
        rounds = map loadRound roundsStr


loadRound :: String -> [Int]
loadRound round = map sum $ transpose $ map loadColor parts
    where
        parts = splitOn (==',') round

-- Expected input : "X Color" 
-- With X being an integer and COlor being either "Red", "Green" or "Blue"
loadColor :: [Char] -> [Int]
loadColor colorData
    | color == "red"    = [read value :: Int, 0, 0]
    | color == "green"  = [0, read value :: Int, 0]
    | color == "blue"   = [0, 0, read value :: Int]
    where
        [value, color] = words colorData





-- Split a list on a given predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'