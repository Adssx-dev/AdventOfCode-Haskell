import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Language.Haskell.TH (newtypeStrategy)


data Card = Card {number :: Int, winnings :: [Int], numbers :: [Int]} deriving (Show, Eq, Ord)

main = do
    handle <- openFile "data/2023/Day4.txt" ReadMode
    contents <- hGetContents handle

    --print $ map parseCard $ lines contents
    print $ part1 $ lines contents
    --print $ part2 $ lines contents

    hClose handle

part1 lines = sum $ map cardScore cards
    where
        cards = map parseCard lines

cardScore :: Card -> Int
cardScore card = score
    where 
        winningNumbers = filter (`elem` winnings card) (numbers card)
        numberOfWinningNumbers = length winningNumbers
        score = if numberOfWinningNumbers == 0
            then 0
            else 2 ^ (numberOfWinningNumbers - 1)

parseCard line = Card{number=cardNumber, winnings=winningNumbers, numbers=cardNumbers}
    where 
        [cardPart, numbersPart] = splitOn (==':') line
        [_, cardNumberStr] = words cardPart
        cardNumber = read cardNumberStr :: Int
        [winningNumbersStr, cardNumbersStr] = splitOn (=='|') numbersPart
        winningNumbers = map read  (words winningNumbersStr) ::[Int]
        cardNumbers = map read (words cardNumbersStr) ::[Int]

-- Split a list on a given predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'