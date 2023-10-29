import System.IO  
import Debug.Trace

main = do  
    handle <- openFile "data/2020/Day2.txt" ReadMode  
    contents <- hGetContents handle  
    print $ length $ filter id $ map passwordValidPt1 $ lines contents
    print $ length $ filter id $ map passwordValidPt2 $ lines contents
    hClose handle  

passwordValidPt1 line = numChars >= minChar && numChars <= maxChar
    where
        (minChar, maxChar, character, password) = parseLine line
        numChars = length $ filter (==character) password


passwordValidPt2 :: [Char] -> Bool
passwordValidPt2 line = (character == fst charsAtIndexes) /= (character == snd charsAtIndexes)
    where
        (start, end, character, password) = parseLine line
        charsAtIndexes = (password!!(start - 1), password!!(end - 1))


parseLine line =  (read min ::Int, read max ::Int , head chars, password)
    where
        [minMax, chars, password] = words line
        min = head $ wordsWhen  (== '-') minMax
        max = head $ tail $ wordsWhen (== '-') minMax 


wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'