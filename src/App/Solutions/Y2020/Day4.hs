module Solutions.Y2020.Day4
( part1
, part2
) where
import System.IO
import Debug.Trace
import Data.List

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ length $ filter id $ map validatePassportsPart1 passports
    where
        passports = parsePassports $ lines inputStr


part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ length $ filter id $ map validatePassportsPart2 passports
    where
        passports = parsePassports $ lines inputStr

validatePassportsPart1 :: [[[Char]]] -> Bool
validatePassportsPart1 pass = length (concat $ filter (/="cid") $ map head pass) == 21

validatePassportsPart2 :: [[[Char]]] -> Bool
validatePassportsPart2 pass = all validatePassportField pass && validatePassportsPart1 pass

validatePassportField :: [String] -> Bool
validatePassportField ["byr", yearStr] = validateInBounds yearStr 1920 2002
validatePassportField ["iyr", yearStr] = validateInBounds yearStr 2010 2020
validatePassportField ["eyr", yearStr] = validateInBounds yearStr 2020 2030
validatePassportField ["hgt", height] = case unit of
        "cm" -> validateInBounds value 150 193
        "in" -> validateInBounds value 59 76
        _ -> False
    where (value, unit) = partition (`elem` ['0'..'9']) height
validatePassportField ["hcl", '#':color] = all (`elem` ['a'..'f'] ++ ['0'..'9']) color && length color == 6
validatePassportField ["hcl", _] = False
validatePassportField ["ecl", color] = color `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validatePassportField ["pid", id] = all (`elem` ['0'..'9']) id && length id == 9
validatePassportField ["cid", _] = True
validatePassportField _ = False


validateInBounds valueStr min max = value >= min && value <= max
    where value = read valueStr


parsePassports :: [[Char]] -> [[[[Char]]]]
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
