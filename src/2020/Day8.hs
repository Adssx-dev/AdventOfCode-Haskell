import System.IO
import Debug.Trace
import Data.List
import System.Directory (Permissions(executable))
import qualified Data.Set as Set



main = do
    handle <- openFile "data/2020/Day8.txt" ReadMode
    contents <- hGetContents handle

    let program = map parseLine (lines contents)
    print $ executeProgram program Set.empty 0 0
    hClose handle

executeProgram :: [(String, Int -> Int)] -> Set.Set Int -> Int -> Int -> Int
executeProgram program history accumulator currentLine = result
    where
        (newAccumulator, newLine) = executeLine (program !! currentLine) (accumulator, currentLine)
        result = if Set.member currentLine history 
            then 
                accumulator 
            else 
                executeProgram program (Set.insert currentLine history) newAccumulator newLine


executeLine :: (String, Int -> Int) -> (Int, Int) -> (Int, Int)
executeLine ("nop", _) (previousAccumulator, previousLine)  = (previousAccumulator, previousLine + 1)
executeLine ("acc", fcn) (previousAccumulator, previousLine) = (fcn previousAccumulator, previousLine + 1)
executeLine ("jmp", fcn) (previousAccumulator, previousLine) = (previousAccumulator, fcn previousLine)
--executeLine (a, _) (_, _) = trace (show a) (0, 0)


parseLine line = (operation, fcn)
    where
        [operation, arg] = words line
        (operator:value) = arg
        valueInt = read value :: Int
        factor = case operator of
            '+' -> 1
            '-' -> -1
        fcn = (+) (valueInt * factor)