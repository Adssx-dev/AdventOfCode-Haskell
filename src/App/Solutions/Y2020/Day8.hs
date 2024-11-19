module Solutions.Y2020.Day8
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import System.Directory (Permissions(executable))
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe


part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ executeProgramPt1 program Set.empty 0 0
    where 
        program = map parseLine (lines inputStr)


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ findRightProgram program 0
    where 
        program = map parseLine (lines inputStr)

alterProgram program targetInstruction = result
    where
        newInstruction = alterInstruction $ program !! targetInstruction
        (before, _:after) = splitAt targetInstruction program
        result = case newInstruction of 
            Just i -> Just $ before ++ i: after
            Nothing -> Nothing


findRightProgram initialProgram instructionToAlter = result
    where
        alteredProgram = alterProgram initialProgram instructionToAlter
        result = case alteredProgram of
            Nothing -> findRightProgram initialProgram (instructionToAlter + 1)
            Just newProgram -> case executeProgramPt2 (Maybe.fromMaybe [] alteredProgram) Set.empty 0 0 of 
                Nothing -> findRightProgram initialProgram (instructionToAlter + 1)
                Just res -> res


alterInstruction ("nop", fcn) = Just ("jmp", fcn)
alterInstruction ("jmp", fcn) = Just ("nop", fcn)
alterInstruction (op, fcn) = Nothing

executeProgramPt2 :: [(String, Int -> Int)] -> Set.Set Int -> Int -> Int -> Maybe Int
executeProgramPt2 program history accumulator currentLine = result
    where
        (newAccumulator, newLine) = executeLine (program !! currentLine) (accumulator, currentLine)
        hasReachedEnd = currentLine == length program
        result = case (hasReachedEnd, Set.member currentLine history) of
            (True, _) -> Just accumulator
            (_, True) -> Nothing
            (_, False) -> executeProgramPt2 program (Set.insert currentLine history) newAccumulator newLine
                

executeProgramPt1 :: [(String, Int -> Int)] -> Set.Set Int -> Int -> Int -> Int
executeProgramPt1 program history accumulator currentLine = result
    where
        (newAccumulator, newLine) = executeLine (program !! currentLine) (accumulator, currentLine)
        result = if Set.member currentLine history 
            then 
                accumulator 
            else 
                executeProgramPt1 program (Set.insert currentLine history) newAccumulator newLine


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