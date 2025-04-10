module Card (module Card) where

import System.Random (StdGen)

newtype Deck = Deck [Card]
  deriving (Show, Eq)

newtype Hand = Hand [Card]
  deriving (Show, Eq)

emptyHand :: Hand
emptyHand = Hand []

addCard :: Card -> Hand -> Hand
addCard card (Hand hand) = Hand (card : hand)

data Card = Card {rank :: Rank, suit :: Suit}
  deriving (Show, Eq, Bounded)

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Eq, Enum, Bounded)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum, Bounded)

fullDeck :: Deck
fullDeck = Deck $ liftA2 Card [minBound .. maxBound] [minBound .. maxBound]

mkShuffledDeck :: StdGen -> Deck
mkShuffledDeck g = fullDeck

value :: Rank -> Int
value r
  | r == Ace = 11 -- Ace as 11
  | r `elem` [Jack, Queen, King] = 10 -- Jack, Queen, King all map to 10
  | otherwise = fromEnum r + 2 -- Ranks 2-10 map to their respective values

draw :: Deck -> Maybe (Card, Deck)
draw (Deck []) = Nothing
draw (Deck (c : cs)) = Just (c, Deck cs)

dealNTo :: Int -> [a] -> Deck -> ([(a, Hand)], Deck)
dealNTo _ [] d = ([], d)
dealNTo n (p : ps) (Deck d) =
  let (h, d') = splitAt n d
      (rest, d'') = dealNTo n ps (Deck d')
   in ((p, Hand h) : rest, d'')

score :: Hand -> Int
score (Hand hand) = adjustForAces total aces
  where
    ranks = map rank hand
    total = sum (map value ranks)
    aces = length (filter (== Ace) ranks)

    adjustForAces t a
      | t <= 21 = t
      | a == 0 = t
      | otherwise = adjustForAces (t - 10) (a - 1)

isBust :: Hand -> Bool
isBust hand = score hand > 21
