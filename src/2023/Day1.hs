
import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe


main = do
    handle <- openFile "data/2023/Day1.txt" ReadMode
    contents <- hGetContents handle


    print $ sumOfAllLinesValues $ lines contents
    print $ sumOfAllLinesValues $ map convertDigitsToLetters $ lines contents


    hClose handle


sumOfAllLinesValues lines = sum $ map extractFirstAndLastDigit lines

extractFirstAndLastDigit line = firstDigit * 10 + lastDigit
    where
        digits = filter (`elem` "1234567890") line
        firstDigit = read (head digits:"") :: Int
        lastDigit = read (last digits:"") :: Int

-- In some cases last letter of a digit is also the first letter of another one
-- In these cases, we add the last digit back to the list to ensure it can be counted twice if necessary
convertDigitsToLetters []=[]
convertDigitsToLetters ('o':'n':'e':xs) =           '1':convertDigitsToLetters ('e':xs)
convertDigitsToLetters ('t':'w':'o':xs) =           '2':convertDigitsToLetters ('o':xs)
convertDigitsToLetters ('t':'h':'r':'e':'e':xs) =   '3':convertDigitsToLetters xs
convertDigitsToLetters ('f':'o':'u':'r':xs) =       '4':convertDigitsToLetters xs
convertDigitsToLetters ('f':'i':'v':'e':xs) =       '5':convertDigitsToLetters xs
convertDigitsToLetters ('s':'i':'x':xs) =           '6':convertDigitsToLetters xs
convertDigitsToLetters ('s':'e':'v':'e':'n':xs) =   '7':convertDigitsToLetters ('n':xs)
convertDigitsToLetters ('e':'i':'g':'h':'t':xs) =   '8':convertDigitsToLetters ('t':xs)
convertDigitsToLetters ('n':'i':'n':'e':xs) =       '9':convertDigitsToLetters ('e':xs)
convertDigitsToLetters (x:xs) =                     x: convertDigitsToLetters xs
