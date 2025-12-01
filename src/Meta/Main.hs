module Main
where

import qualified Data.Text as T
import System.IO
import System.FilePath
import System.Directory
import Control.Monad
import System.Directory.Internal.Prelude (getArgs)

dataFolder = "data/"
templateFolder = "templates/"

templateReplacements year day = foldr ((.) . uncurry T.replace) id replacementList
    where
        replacementList =
            [ (T.pack "{{YEAR}}", T.pack $ show year)
            , (T.pack "{{DAY}}",  T.pack $ show day)]

loadDayTemplateFile = do
        handle <- openFile path ReadMode
        hGetContents handle
    where
        path = templateFolder </> "dayTemplate.txt"

loadYearTemplateFile = do
        handle <- openFile path ReadMode
        hGetContents handle
    where
        path = templateFolder </> "yearTemplate.txt"

createDayFile template year day = do
        let dayPath = generateDayPath year day
        let filledTemplate = templateReplacements year day template
        fileExists <- doesFileExist dayPath
        unless fileExists $ writeFile dayPath $ T.unpack filledTemplate
        where
            generateDayPath year day = "src" </> "App" </> "Solutions" </> ("Y" ++ show year) </> ("Day" ++ show day) <.> ".hs"

createYearFile year = do
    let yearPath = generateYearPath year 
    yearTemplate <- loadYearTemplateFile
    let filledTemplate = templateReplacements year 0 (T.pack yearTemplate) -- no day in this template, the second value does not matter 
    fileExists <- doesFileExist yearPath
    unless fileExists $ writeFile yearPath $ T.unpack filledTemplate
    where
        generateYearPath year = "src" </> "App" </> "Solutions" </> ("Y" ++ show year) <.> ".hs"

generateFolder year = do
    let yearPath = generateYearPath year
    directoryExists <- doesDirectoryExist yearPath
    unless directoryExists $ createDirectory yearPath
    where
        generateYearPath year = "src" </> "App" </> "Solutions" </> ("Y" ++ show year)



main = do
    putStrLn "hello"
    args <- getArgs
    let year = read (head args) :: Int

    generateFolder year
    dayTemplate <- loadDayTemplateFile
    mapM_ (createDayFile (T.pack dayTemplate) year) [1..12] 
    createYearFile year


