import System.IO
import Debug.Trace


main = do
    handle <- openFile "data/2020/Day4.txt" ReadMode
    contents <- hGetContents handle
    let passports = parsePassports $ lines contents
    print $ length $ filter id $ map validatePassports passports

    hClose handle

validatePassportsPart1 pass = length (concat $ filter (/="cid") $ map head pass) == 21

parsePassports textLines = map parseOnePassport passports
    where
        passports = splitOn (== []) textLines

parseOnePassport pass = map (splitOn (==':')) $ concatMap words pass

-- Split a list on a given predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s = case dropWhile p s of
    [] -> []
    s' -> w : splitOn p s''
        where (w, s'') = break p s'
