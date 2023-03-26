module Blackjack where

import Cards
import RunGame

aCard1 :: Card
aCard1 = Card King Spades -- define your favorite card here

aCard2 :: Card
aCard2 = Card (Numeric 7) Hearts -- define another card here

aHand :: Hand
aHand = [aCard1, aCard2] -- a Hand with two Cards, aCard1 and aCard2

-- Task A1

hand2 :: Hand
hand2 = Card (Numeric 2) Hearts : (Card Jack Spades : [])

sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 1 + 1 + size []
            , 1 + 1 + 0
            , 2
            ]

-- Task A2

-- Returns the string representation of a hand on multiple lines
display :: Hand -> String
display hand = unlines (map displayCard hand)

-- Converts a Suit to its corresponding unicode character.
unicodeSuit :: Suit -> String
unicodeSuit s = case s of 
    Hearts -> "\9829"
    Spades -> "\9824"
    Diamonds -> "\9830"
    Clubs -> "\9827"

-- Returns the string representation of a card, formatted
formatCard :: String -> Suit -> String
formatCard r s = r ++ " of " ++ (unicodeSuit s)

-- Returns the string representation of a card
displayCard :: Card -> String
displayCard (Card (Numeric n) s) = formatCard (show n) s
displayCard (Card r s) = formatCard (show r) s

-- Prints a hand
printHand :: Hand -> IO()
printHand = putStrLn . display

-- Task A3

hand3 :: Hand
hand3 = [Card (Numeric 2) Hearts, Card Jack Spades, Card King Diamonds, Card Ace Clubs]

hand4 :: Hand
hand4 = [Card Jack Spades, Card Ace Clubs, Card (Numeric 2) Clubs]

valueRank :: Rank -> Int
valueRank Ace = error "Superposition not collapsed yet"
valueRank (Numeric n) = n
valueRank _ = 10

valueCard :: Card -> Int
valueCard c = valueRank $ rank c

numberOfAces :: Hand -> Int
numberOfAces hand = length $ filter (\x -> rank x == Ace) hand

value :: Hand -> Int
value hand = if value11 <= 21 then value11 else value1
    where
        numAces = numberOfAces hand
        baseHand = filter (\x -> rank x /= Ace) hand
        baseHandValue = sum (map valueCard baseHand)
        value11 = baseHandValue + numAces * 11
        value1 = baseHandValue + numAces

--value2 :: Hand
value2 hand = min (baseHandValue + aces * 11) (baseHandValue + aces)
  where
    baseHandValue = sum [valueCard card | card <- hand, rank card /= Ace]
    aces = numberOfAces hand

-- Task A4

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

winner :: Hand -> Hand -> Player
winner guest bank
    | gameOver guest           = Bank
    | gameOver bank            = Guest
    | value guest > value bank = Guest
    | otherwise                = Bank
