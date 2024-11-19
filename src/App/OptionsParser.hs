module OptionsParser 
where

import Options.Applicative

data ProgramOptions = ProgramOptions
    { year :: Int
    , day :: Maybe Int
    , part :: Maybe Int
    } deriving Show
    
getProgramOptions :: IO ProgramOptions
getProgramOptions = execParser $ info globalParser mempty

globalParser = 
    ProgramOptions
        <$> yearParser
        <*> dayParser
        <*> partParser

yearParser :: Parser Int
yearParser =
    option auto $
        short 'y'
            <> long "year"
            <> metavar "Int"
            <> help "Year to run"
        
dayParser :: Parser (Maybe Int)
dayParser =
    optional $ option auto $
        short 'd'
            <> long "day"
            <> metavar "Int"
            <> help "Day to run in the given year"

partParser :: Parser (Maybe Int)
partParser =
    optional $ option auto $
        short 'p'
            <> long "part"
            <> metavar "Int"
            <> help "Part to run"