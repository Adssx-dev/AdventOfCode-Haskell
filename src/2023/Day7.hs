import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char

data HandType = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind |FiveOfAKind  deriving (Show, Eq, Ord)
data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord)
data Hand = Hand {cards :: [Card], handType :: HandType, bid :: Int}  deriving (Show, Eq, Ord)

main = do
    handle <- openFile "data/2023/Day7.txt" ReadMode
    contents <- hGetContents handle

    print $ part1 (lines contents)
    --print $ part2 (lines contents)



    hClose handle

part1 lines = sum $ zipWith (\x y -> x * bid y) [1..] $ sortBy compareHands hands
    where
        hands = map loadHand lines



compareHands a b = if handType a == handType b
    then compare (cards a ) (cards b)
    else compare (handType a) (handType b)



loadHand line = Hand {cards, handType, bid}
    where
        [cardsStr, bidStr] = words line
        cards = map charToCard cardsStr
        bid = read bidStr :: Int
        cardsOrdered = sortBy (\(a,_) (b,_) -> compare b a) $ map (\x -> (length x, head x)) $  group $ sort cards
        handType = calculateHandType cardsOrdered

calculateHandType ((5, _):xs) = FiveOfAKind
calculateHandType ((4, _):xs) = FourOfAKind
calculateHandType ((3, _):(2,_):xs) = FullHouse
calculateHandType ((3, _):xs) = ThreeOfAKind
calculateHandType ((2, _):(2,_):xs) = TwoPairs
calculateHandType ((2, _):xs) = OnePair
calculateHandType _ = HighCard

charToCard '2' = Two
charToCard '3' = Three
charToCard '4' = Four
charToCard '5' = Five
charToCard '6' = Six
charToCard '7' = Seven
charToCard '8' = Eight
charToCard '9' = Nine
charToCard 'T' = Ten
charToCard 'J' = Jack
charToCard 'Q' = Queen
charToCard 'K' = King
charToCard 'A' = Ace