{-# LANGUAGE BlockArguments #-}

module Domain (module Domain) where

import Data.Map qualified as Map
import Data.String (IsString)
import Data.Vector qualified as V
import System.Random (StdGen, randomR)

type Chips = Int

newtype PlayerId = PlayerId String
  deriving (Eq, Ord, Show, Read, IsString)

data Player = Player
  { hand :: Hand,
    bet :: Bet,
    hasStood :: Bool
  }
  deriving (Show)

data Bet = Bet
  { current :: Chips,
    chips :: Chips
  }
  deriving (Eq, Show)

data Outcome
  = Win Chips
  | Loss Chips
  | Push
  deriving
    ( -- | BlackjackWin Chips
      -- | Surrendered Chips
      Eq,
      Show
    )

data Command
  = JoinGame PlayerId
  | LeaveGame PlayerId
  | StartGame
  | PlaceBet PlayerId Chips
  | DealInitialCards
  | PlayerHit PlayerId
  | PlayerStand PlayerId
  | PlayerDoubleDown PlayerId
  | DealerPlay
  | ResolveRound
  | RestartGame
  | ExitGame
  -- \| PlayerSplit PlayerId
  -- \| PlayerSurrender PlayerId
  deriving (Read)

data Event
  = PlayerJoined PlayerId
  | PlayerLeft PlayerId
  | GameStarted
  | BetPlaced PlayerId Chips
  | CardsDealt [(PlayerId, Hand)] Hand
  | PlayerHitCard PlayerId Card
  | PlayerStood PlayerId
  | PlayerDoubledDown PlayerId Card
  | DealerPlayed Hand
  | RoundResolved (Map.Map PlayerId (Outcome, Bet))
  | GameRestarted
  | GameExited
  deriving
    ( -- | PlayerSplitHand PlayerId Card Card
      -- | PlayerSurrendered PlayerId
      Eq,
      Show
    )

data GameError
  = PlayerAlreadyJoined
  | GameAlreadyStarted
  | PlayerNotFound
  | TooFewPlayers
  | BadCommand
  | MalsizedBet
  | PlayerAlreadyBet
  | EmptyDeck
  | PlayersStillBetting
  | PlayersStillPlaying
  deriving (Eq, Show)

data Deck = Deck
  { gen :: StdGen,
    drawn :: Int
  }
  deriving (Eq, Show)

mkDeck :: StdGen -> Deck
mkDeck g = Deck g 0

-- Efficient O(1) lookup of the n-th element of a virtual Fisher-Yates shuffle
fisherYatesIndex :: [a] -> Int -> StdGen -> Maybe a
fisherYatesIndex xs n g
  | n >= length xs = Nothing
  | otherwise = go (V.fromList xs) 0 g
  where
    go v i g0
      | i > n = Just (v V.! n)
      | otherwise =
          let (j, g') = randomR (i, V.length v - 1) g0
              v' = v V.// [(i, v V.! j), (j, v V.! i)]
           in go v' (i + 1) g'

drawCard :: Deck -> Maybe (Card, Deck)
drawCard deck@Deck {gen, drawn} = do
  card <- fisherYatesIndex allCards drawn gen
  pure (card, deck {drawn = drawn + 1})

dealN :: Int -> Deck -> Maybe (Hand, Deck)
dealN n deck = go n deck []
  where
    go 0 d acc = Just (Hand acc, d)
    go k d acc = do
      (c, d') <- drawCard d
      go (k - 1) d' (c : acc)

dealNTo :: Int -> [a] -> Deck -> Maybe ([(a, Hand)], Deck)
dealNTo _ [] deck = Just ([], deck)
dealNTo n (p : ps) deck = do
  (hand, deck') <- dealN n deck
  (rest, deck'') <- dealNTo n ps deck'
  pure ((p, hand) : rest, deck'')

newtype Hand = Hand [Card]
  deriving (Show, Eq)

emptyHand :: Hand
emptyHand = Hand []

handSize :: Hand -> Int
handSize (Hand hand) = length hand

addCard :: Card -> Hand -> Hand
addCard card (Hand hand) = Hand (card : hand)

data Card = Card {rank :: Rank, suit :: Suit}
  deriving (Show, Eq, Bounded)

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Eq, Enum, Bounded)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum, Bounded)

allCards :: [Card]
allCards = liftA2 Card [minBound .. maxBound] [minBound .. maxBound]

value :: Rank -> Int
value r
  | r == Ace = 11 -- Ace as 11
  | r `elem` [Jack, Queen, King] = 10 -- Jack, Queen, King all map to 10
  | otherwise = fromEnum r + 2 -- Ranks 2-10 map to their respective values

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
