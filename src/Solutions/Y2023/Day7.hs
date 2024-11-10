import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char

-- Please, I don't wanna open this file at any point in the future, even when writing it I can barely understand it

data HandType = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind |FiveOfAKind  deriving (Show, Eq, Ord)
data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord)
data Hand = Hand {cards :: [Card], handType :: HandType, bid :: Int}  deriving (Show, Eq, Ord)

main = do
    handle <- openFile "data/2023/Day7.txt" ReadMode
    contents <- hGetContents handle

    print $ part1 (lines contents)
    print $ part2 (lines contents)

    hClose handle


part1 lines = sum $ zipWith (\x y -> x * bid y) [1..] $ sortBy (compareHands compare) hands
    where
        hands = map (loadHand calculateHandType) lines

part2 lines = sum $ zipWith (\x y -> x * bid y) [1..] $  sortBy (compareHands compareDeckPt2) hands
    where
        hands = map (loadHandPt2 calculateHandTypePt2) lines

-- Compare two hands for part 1
compareHands :: ([Card] -> [Card] -> Ordering) -> Hand -> Hand -> Ordering
compareHands cmpFunction a b = if handType a == handType b
    then cmpFunction (cards a ) (cards b)
    else compare (handType a) (handType b)


-- Compare two decks of cards for part 2 where the Jack has the lowest value
compareDeckPt2 [] [] = EQ
compareDeckPt2 (a:xsa) (b:xsb) = case compareCardPt2 a b of
    LT -> LT
    GT -> GT
    EQ -> compareDeckPt2 xsa xsb

-- Used to set the jack at the begining of the list before computing the hand type
compareJackFirst :: (Int, Card) -> (Int, Card) -> Ordering
compareJackFirst a b = case (snd a, snd b) of
    (Jack, Jack) -> EQ
    (Jack, _) -> LT
    (_, Jack) ->  GT
    (x, y) -> EQ

-- Compare two cards for part, the Jack is now the lowest card, all others keep their value
compareCardPt2 :: Card -> Card -> Ordering
compareCardPt2 a b = case (a, b) of
    (Jack, Jack) -> EQ
    (Jack, _) -> LT
    (_, Jack) ->  GT
    (x, y) -> compare x y

-- Load the hand for part 1
loadHand handTypeCalculator line = Hand {cards, handType, bid}
    where
        [cardsStr, bidStr] = words line
        cards = map charToCard cardsStr
        bid = read bidStr :: Int
        cardsOrdered = sortBy (\(a,_) (b,_) -> compare b a) $ map (\x -> (length x, head x)) $  group $ sort cards
        handType = handTypeCalculator cardsOrdered

-- Load hand for part 2. The only difference is now we put the jack at the begining of the list to help for pattern matching in calculateHandTypePt2
loadHandPt2 handTypeCalculator line = Hand {cards, handType, bid}
    where
        [cardsStr, bidStr] = words line
        cards = map charToCard cardsStr
        bid = read bidStr :: Int
        cardsOrdered = sortBy compareJackFirst $ sortBy (\(a,_) (b,_) -> compare b a) $ map (\x -> (length x, head x)) $  group $ sort cards
        handType = handTypeCalculator cardsOrdered

-- Calculate which type of hand we have (input must be sorted by decreasing  number of each type of card)
calculateHandType ((5, _):xs) = FiveOfAKind
calculateHandType ((4, _):xs) = FourOfAKind
calculateHandType ((3, _):(2,_):xs) = FullHouse
calculateHandType ((3, _):xs) = ThreeOfAKind
calculateHandType ((2, _):(2,_):xs) = TwoPairs
calculateHandType ((2, _):xs) = OnePair
calculateHandType _ = HighCard

-- UGLYYYYYYYYYY
-- Same a before but with all combinations with jacks included
calculateHandTypePt2 ((5, _):xs) = FiveOfAKind
calculateHandTypePt2 ((1, Jack):(4, _):xs) = FiveOfAKind
calculateHandTypePt2 ((2, Jack):(3, _):xs) = FiveOfAKind
calculateHandTypePt2 ((3, Jack):(2, _):xs) = FiveOfAKind
calculateHandTypePt2 ((4, Jack):(1, _):xs) = FiveOfAKind
calculateHandTypePt2 ((4, _):xs) = FourOfAKind
calculateHandTypePt2 ((1, Jack):(3, _):xs) = FourOfAKind
calculateHandTypePt2 ((2, Jack):(2, _):xs) = FourOfAKind
calculateHandTypePt2 ((3, Jack):(1, _):xs) = FourOfAKind
calculateHandTypePt2 ((3, _):(2,_):xs) = FullHouse
calculateHandTypePt2 ((1, Jack):(2, _):(2,_):xs) = FullHouse
calculateHandTypePt2 ((3, _):xs) = ThreeOfAKind
calculateHandTypePt2 ((1, Jack):(2, _):xs) = ThreeOfAKind
calculateHandTypePt2 ((2, Jack):(1, _):xs) = ThreeOfAKind
calculateHandTypePt2 ((2, _):(2,_):xs) = TwoPairs
calculateHandTypePt2 ((2, _):xs) = OnePair
calculateHandTypePt2 ((1, Jack):(1, _):xs) = OnePair
calculateHandTypePt2 _ = HighCard

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