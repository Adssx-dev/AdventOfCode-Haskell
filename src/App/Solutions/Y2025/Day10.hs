module Solutions.Y2025.Day10
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Utils.List (splitOn, indicesToBooleanList, removeFirstAndLast)
import Utils.Math (xor)
import Utils.Combinatorics (generateAllCombinations)

-----------
-- Types --
-----------
-- Lights
type Light = Bool

-- True for On, False for Off
type LightArray = [Light]

-- Light affected by a button
type Button = [Light]

type JoltageRequirements = [Int]

data Machine = Machine
    { lights :: LightArray
    , buttons :: [Button]
    , joltages :: JoltageRequirements
    } deriving(Eq, Show)

-------------
-- Parsing --
-------------
parseLight '.' = False
parseLight '#' = True
parseLight c = error $ "Character '" ++ show c ++ " does not match a light pattern"

-- Remove first and last elements that are square brackets
parseLightArray :: String -> LightArray
parseLightArray lightsStr = map parseLight (removeFirstAndLast lightsStr)

parseButton :: String -> [Bool]
parseButton buttonStr = indicesToBooleanList indices 0
    where
        indices = map read $ splitOn (==',') (removeFirstAndLast buttonStr) :: [Int]

parseJoltageRequirement :: String -> JoltageRequirements
parseJoltageRequirement joltagesStr = map read $ splitOn (==',') (removeFirstAndLast joltagesStr) :: JoltageRequirements

parseMachine :: String -> Machine
parseMachine lineStr = Machine{lights = lights, buttons=buttons, joltages=joltageRequirements}
    where
        groups = splitOn (==' ') lineStr
        lights = parseLightArray $ head groups
        buttons = map parseButton (removeFirstAndLast groups)
        joltageRequirements = parseJoltageRequirement $ last groups

-------------
-- Solving --
-------------

solveMachinePt1 :: Machine -> Int
solveMachinePt1 machine = minimum $ map snd combinationsSolvingMachine
    where
        buttonCombinations = generateAllCombinations (buttons machine)
        combinationsResults = map (\bc -> (simulateMultipleButtonPressings bc, length bc)) buttonCombinations
        combinationsSolvingMachine = filter (\candidate -> compareLights (fst candidate) (lights machine)) combinationsResults

--simulateButtonPressing previousState buttons newState = 
simulateButtonPressing :: LightArray -> Button -> LightArray -> LightArray
simulateButtonPressing [] [] newState = reverse newState
simulateButtonPressing [] (b:bs) newState = simulateButtonPressing [] bs (b:newState)
simulateButtonPressing (l:ls) [] newState = simulateButtonPressing ls [] (l:newState)
simulateButtonPressing (l:ls) (b:bs) newState = simulateButtonPressing ls bs (xor l b:newState)

simulateMultipleButtonPressings :: Foldable t => t Button -> LightArray
simulateMultipleButtonPressings = foldl (\l b -> simulateButtonPressing l b []) []

-- One series can be longer iif the longest values are all false
compareLights :: LightArray -> LightArray -> Bool
compareLights [] [] = True
compareLights [] (False:l2s) = compareLights [] l2s
compareLights [] (True:l2s) = False
compareLights (False:l1s) [] = compareLights l1s []
compareLights (True:l1s) [] = False
compareLights (l1:l1s) (l2:l2s) = l1 == l2 && compareLights l1s l2s

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ sum $ map solveMachinePt1 machines
    where 
        machines = map parseMachine (lines inputStr)


part2 :: [Char] -> Maybe Int
part2 inputStr = Nothing
