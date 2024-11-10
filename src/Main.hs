module Main where

import Solutions.Y2015
import Data.Map as Map

import OptionsParser


globalMap :: Map Int (Map Int ([Char] -> Maybe Int, [Char] -> Maybe Int))
globalMap = Map.fromList . zip [2015..] $
    [ Solutions.Y2015.dayMap
    ]

main = getProgramOptions >>= print


-- main = print $ case fcn of
--     Just f -> show $ f "))(())"
--     Nothing -> "Could not find function"
--     where
--         fcn = fst <$> Map.lookup 1 Solutions.Y2015.dayMap



getSolutionFunction year day part
    | part == 1 = fst <$> getSolutionFunctionInternal year day
    | part == 2 = snd <$> getSolutionFunctionInternal year day
    | otherwise = error ("Days only have part 1 and part 2, but part " ++ show part ++ "was requested")
    where
        getSolutionFunctionInternal year day = case selectedYear of
            Just y -> Map.lookup day y
            Nothing -> error ("Could not find requester year " ++ show year ++ ". Available years are : " ++ show (Map.keys globalMap))
            where
                selectedYear = Map.lookup year globalMap
                --selectedDay = Map.lookup day selectedYear

getDayInput year day = "data/" ++ show year ++ "/"

