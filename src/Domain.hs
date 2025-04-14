{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Domain (module Domain) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.Zipper qualified as Z
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V
import System.Random (StdGen, randomR)

type Chips = Int

newtype PlayerId = PlayerId Int
  deriving (Eq, Ord, Show, Read)

data Player = Player
  { id :: PlayerId,
    stack :: PlayerStack,
    name :: Text
  }
  deriving (Eq, Show)

newPlayer :: PlayerId -> Text -> Player
newPlayer pid = Player pid stack
  where
    stack = PlayerStack (Bet 0) 100

data PlayerStack = PlayerStack
  { currentBet :: Bet,
    chips :: Chips
  }
  deriving (Eq, Show)

data PlayerSession = PlayerSession
  { player :: Player,
    hands :: Z.Zipper HandState,
    insurance :: Maybe InsuranceChoice,
    hasSurrendered :: Bool
  }
  deriving (Eq, Show)

initPlayerSession :: Hand -> Player -> PlayerSession
initPlayerSession hand player =
  let handState = initHandState hand
   in PlayerSession player (Z.fromNonEmpty $ pure handState) Nothing False

hasCompletedTurn :: PlayerSession -> Bool
hasCompletedTurn PlayerSession {hasSurrendered, hands} =
  hasSurrendered || all (\h -> hasStood h || hasDoubledDown h) hands

withSession ::
  PlayerId ->
  Map.Map PlayerId PlayerSession ->
  (PlayerSession -> Either GameError a) ->
  Either GameError a
withSession pid sessions f =
  maybe (Left PlayerSessionNotFound) f (Map.lookup pid sessions)

data HandState = HandState
  { hand :: Hand,
    bet :: Bet,
    hasStood :: Bool,
    hasDoubledDown :: Bool
  }
  deriving (Eq, Show)

initHandState :: Hand -> HandState
initHandState hand = HandState hand (Bet 0) False False

newtype Dealer = Dealer Hand
  deriving (Eq, Show)

visibleCard :: Dealer -> Card
visibleCard (Dealer (Hand hand)) = head hand -- assuming dealer shows first card

data InsuranceChoice
  = TookInsurance Chips
  | DeclinedInsurance
  deriving (Eq, Show)

newtype Bet = Bet {current :: Chips}
  deriving (Eq, Ord, Show, Read, Num)

withValidBet :: Bet -> Chips -> (Bet -> Either GameError a) -> Either GameError a
withValidBet bet chips k
  | bet <= 0 || current bet > chips = Left MalsizedBet
  | otherwise = k bet

data Command
  = JoinGame Text
  | LeaveGame PlayerId
  | StartGame
  | PlaceBet PlayerId Bet
  | DealInitialCards
  | Hit PlayerId
  | Stand PlayerId
  | DoubleDown PlayerId
  | Split PlayerId
  | Surrender PlayerId
  | TakeInsurance PlayerId Bet
  | RejectInsurance PlayerId
  | ResolveInsurance
  | DealerPlay
  | ResolveRound
  | RestartGame
  | ExitGame
  deriving (Read)

data Event
  = PlayerJoined PlayerId Text
  | PlayerLeft PlayerId
  | GameStarted
  | BetPlaced PlayerId Bet
  | CardsDealt [(PlayerId, Hand)] Dealer
  | PlayerTookInsurance PlayerId Chips
  | PlayerDeclinedInsurance PlayerId
  | InsuranceResolved (Map.Map PlayerId InsurancePayout)
  | HitCard PlayerId Card
  | PlayerStood PlayerId
  | PlayerDoubledDown PlayerId Card
  | PlayerSplitHand PlayerId Card Card Card Card
  | PlayerSurrendered PlayerId
  | DealerPlayed Dealer
  | RoundResolved DealerOutcome (Map.Map PlayerId PlayerSummary)
  | GameRestarted
  | GameExited
  deriving (Eq, Show)

data DealerOutcome
  = DealerBlackjack
  | DealerBust
  | DealerFinalScore Int
  deriving (Eq, Show)

determineDealerOutcome :: Dealer -> DealerOutcome
determineDealerOutcome (Dealer hand)
  | isBlackjack hand = DealerBlackjack
  | isBust hand = DealerBust
  | otherwise = DealerFinalScore (score hand)

data PlayerSummary = PlayerSummary
  { handOutcomes :: NonEmpty Outcome,
    netChipChange :: Int,
    finalChips :: Chips,
    nextRoundBet :: Bet,
    insurancePayout :: Maybe InsurancePayout
  }
  deriving (Eq, Show)

data Outcome
  = PlayerWins WinReason
  | DealerWins LossReason
  | Push
  deriving (Eq, Show)

data WinReason
  = Blackjack
  | OutscoredDealer
  deriving (Eq, Show)

data LossReason
  = PlayerBust
  | Surrendered
  | OutscoredByDealer
  deriving (Eq, Show)

determineOutcome :: PlayerSession -> HandState -> DealerOutcome -> Outcome
determineOutcome PlayerSession {hasSurrendered} HandState {hand} = \case
  DealerBlackjack
    | isBlackjack hand -> Push
    | otherwise -> DealerWins OutscoredByDealer
  DealerBust
    | hasSurrendered -> DealerWins Surrendered -- Surrender always results in dealer win
    | isBust hand -> DealerWins PlayerBust -- Both busting = dealer wins
    | otherwise -> PlayerWins OutscoredDealer
  DealerFinalScore dealerScore
    | hasSurrendered -> DealerWins Surrendered -- Surrender always results in dealer win
    | isBlackjack hand -> PlayerWins Blackjack
    | isBust hand -> DealerWins PlayerBust
    | otherwise -> case compare (score hand) dealerScore of
        GT -> PlayerWins OutscoredDealer
        LT -> DealerWins OutscoredByDealer
        EQ -> Push

chipsDelta :: Bet -> Outcome -> Chips
chipsDelta Bet {current} = \case
  PlayerWins Blackjack -> payout 1.5
  PlayerWins _ -> payout 1.0
  DealerWins Surrendered -> -(current `div` 2) -- Player loses half their bet when surrendering
  DealerWins _ -> -current
  Push -> 0
  where
    payout m = floor @Float (fromIntegral current * m)

data InsurancePayout
  = WonInsurancePayout Chips
  | LostInsuranceBet Chips
  | NoInsurance
  deriving (Eq, Show)

payoutForInsurance :: Dealer -> PlayerSession -> InsurancePayout
payoutForInsurance (Dealer dealerHand) PlayerSession {insurance} = case insurance of
  Just (TookInsurance amt)
    | isBlackjack dealerHand -> WonInsurancePayout (amt * 2) -- 2:1 insurance payout if the dealer has a blackjack
    | otherwise -> LostInsuranceBet amt
  _ -> NoInsurance

data GameError
  = PlayerAlreadyJoined
  | GameAlreadyStarted
  | PlayerNotFound
  | PlayerSessionNotFound
  | TooFewPlayers
  | PlayerAlreadyActed
  | BadCommand
  | MalsizedBet
  | PlayerAlreadyBet
  | PlayerAlreadyInsured
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

extractPair :: Hand -> Maybe (Card, Card)
extractPair (Hand (a : b : _)) | rank a == rank b = Just (a, b)
extractPair _ = Nothing

data Card = Card {rank :: Rank, suit :: Suit}
  deriving (Show, Eq, Bounded)

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Eq, Enum, Bounded)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum, Bounded)

allCards :: [Card]
allCards = liftA2 Card [minBound .. maxBound] [minBound .. maxBound]

isAce :: Card -> Bool
isAce (Card Ace _) = True
isAce _ = False

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

isBlackjack :: Hand -> Bool
isBlackjack hand = score hand == 21 && handSize hand == 2