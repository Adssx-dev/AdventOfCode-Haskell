module Main where

import Solutions.Y2015
import Solutions.Y2016
import Solutions.Y2017
import Solutions.Y2018
import Solutions.Y2019
import Solutions.Y2020
import Solutions.Y2021
import Solutions.Y2022
import Solutions.Y2023
import Solutions.Y2024
import Solutions.Y2025
import qualified Data.Map as Map

import OptionsParser
import System.FilePath.Posix
import System.IO


data LogEntry = LogEntry {
  year :: Int
, day :: Int
, part :: Int
, expectedResult :: Maybe Int
, actualResult :: Maybe Int
} deriving (Show, Eq, Ord)

globalMap :: Map.Map Int (Map.Map Int ([Char] -> Maybe Int, [Char] -> Maybe Int))
globalMap = Map.fromList . zip [2015..] $
    [ Solutions.Y2015.dayMap
    , Solutions.Y2016.dayMap
    , Solutions.Y2017.dayMap
    , Solutions.Y2018.dayMap
    , Solutions.Y2019.dayMap
    , Solutions.Y2020.dayMap
    , Solutions.Y2021.dayMap
    , Solutions.Y2022.dayMap
    , Solutions.Y2023.dayMap
    , Solutions.Y2024.dayMap
    , Solutions.Y2025.dayMap
    ]

main = do
    opts <- getProgramOptions
    print opts
    res <- case (OptionsParser.day opts, OptionsParser.part opts) of
            (Just d, Just p) -> runDayPart (OptionsParser.year opts) d p
            (Just d, Nothing) -> runDay (OptionsParser.year opts) d
            _ -> runYear (OptionsParser.year opts)

    print res

runYear :: Int -> IO [LogEntry]
runYear year = concat <$> traverse (runDay year) [1..25]


runDay :: Int -> Int -> IO [LogEntry]
runDay year day = concat <$> traverse (runDayPart year day) [1, 2]

runDayPart :: Int -> Int -> Int -> IO [LogEntry]
runDayPart year day part = do
    solutions <- getDaySolutions year day
    let partSolution = case part of
            1 -> fst solutions
            2 -> snd solutions
            _ -> error $ "Requested invalid part : " ++ show part
    input <- getDayInput year day
    let calculatedResult = case fcn of
            (Just f) -> f  input
            Nothing -> Nothing
    let result = [LogEntry {Main.year=year, Main.day=day, Main.part=part, expectedResult=partSolution, actualResult=calculatedResult}]
    return result
    where
        fcn = getSolutionFunction year day part



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

getDayInput :: (Show p, Show a) => p -> a -> IO String
getDayInput year day = do
        handle <- openFile path ReadMode
        hGetContents handle
    where
        path = "data/" </> show year </> "Day" ++ show day <.> ".txt"

getDaySolutions :: Int -> Int -> IO (Maybe Int, Maybe Int)
getDaySolutions year day = do
        handle <- openFile path ReadMode
        contents <- hGetContents handle
        let result = fmap (map read) $  words <$> safeHead (drop (day - 1) (lines contents)) :: Maybe [Int]
        return (flattenMaybe $ fmap safeHead result, flattenMaybe $ safeHead . drop 1 <$> result)
        where
            path = "data/" </> show year </> "Answers.txt"

-- Get day's answer safely
-- words <$> safeHead $ drop 2 ["1 2", "3 4", "5 6"]

-- Read day's answer safely
-- fmap (map read) $  words <$> ( safeHead $ drop 2 ["1 2", "3 4", "5 6"]) :: Maybe [Int]

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (a:as) = Just a

flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe Nothing = Nothing
flattenMaybe (Just Nothing) = Nothing
flattenMaybe (Just (Just x)) = Just x


