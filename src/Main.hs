module Main where

import Solutions.Y2015
import qualified Data.Map as Map

import OptionsParser
import System.FilePath.Posix
import System.IO


globalMap :: Map.Map Int (Map.Map Int ([Char] -> Maybe Int, [Char] -> Maybe Int))
globalMap = Map.fromList . zip [2015..] $
    [ Solutions.Y2015.dayMap
    ]

main = do
    opts <- getProgramOptions
    print opts
    let fcn = case (day opts, part opts) of
            (Nothing, _) -> error "No day provided" 
            (_, Nothing) -> error "No part provided"
            (Just d, Just p) -> getSolutionFunction (year opts) d p

    input <- getDayInput (year opts) (day opts)
    print $ case fcn of
        Nothing -> error "Could not find function"
        Just f ->  f input


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

getDayInput _ Nothing = error "Could not get day input as day is not defined"
getDayInput year (Just day) = do
        handle <- openFile path ReadMode
        hGetContents handle
    where
        path = "data/" </> show year </> "Day" ++ show day <.> ".txt"

getDaySolutions :: Int -> Maybe Int -> IO (Maybe Int, Maybe Int)
getDaySolutions _ Nothing = error "Could not get day input as day is not defined"
getDaySolutions year (Just day) = do
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


