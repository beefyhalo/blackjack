{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Domain (module Domain) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.Zipper qualified as Z
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V
import System.Random (StdGen, randomR)
import Prelude hiding (round)

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

data PlayerRound = PlayerRound
  { player :: Player,
    hands :: Z.Zipper HandState,
    insurance :: Maybe InsuranceChoice,
    hasSurrendered :: Bool
  }
  deriving (Eq, Show)

hasLost :: PlayerRound -> Bool
hasLost PlayerRound {hands, hasSurrendered} =
  hasSurrendered || all (isBust . hand) hands

hasCompletedTurn :: PlayerRound -> Bool
hasCompletedTurn round@PlayerRound {hands} =
  hasLost round || all (\h -> hasStood h || hasDoubledDown h) hands

initPlayerRound :: Hand -> Player -> PlayerRound
initPlayerRound hand player =
  let handState = initHandState hand
   in PlayerRound player (Z.fromNonEmpty $ pure handState) Nothing False

modifyCurrentHand :: (HandState -> HandState) -> PlayerRound -> PlayerRound
modifyCurrentHand f round@PlayerRound {hands} =
  round {hands = Z.replace (f (Z.current hands)) hands}

withPlayerRound ::
  PlayerId ->
  Map.Map PlayerId PlayerRound ->
  (PlayerRound -> Either GameError a) ->
  Either GameError a
withPlayerRound pid rounds f =
  maybe (Left PlayerRoundNotFound) f (Map.lookup pid rounds)

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

-- Hit if the dealer has less than 17 points
dealerShouldHit :: Dealer -> Bool
dealerShouldHit (Dealer hand) = score hand < 17

data InsuranceChoice
  = TookInsurance Bet
  | DeclinedInsurance
  deriving (Eq, Show)

newtype Bet = Bet {current :: Chips}
  deriving (Eq, Ord, Show, Read, Num)

withValidBet :: Bet -> Chips -> (Bet -> Either GameError a) -> Either GameError a
withValidBet bet chips f
  | bet <= 0 || current bet > chips = Left MalsizedBet
  | otherwise = f bet

data Command
  = BettingCmd BettingCommand
  | DealerTurnCmd DealerTurnCommand
  | DealingCmd DealingCommand
  | InsuranceCmd InsuranceCommand
  | LobbyCmd LobbyCommand
  | PlayerTurnCmd PlayerTurnCommand
  | ResolutionCmd ResolutionCommand
  | ResultCmd ResultCommand
  | ExitGame
  deriving (Eq, Read, Show)

data Event
  = BettingEvt BettingEvent
  | DealerTurnEvt DealerTurnEvent
  | DealingEvt DealingEvent
  | InsuranceEvt InsuranceEvent
  | LobbyEvt LobbyEvent
  | PlayerTurnEvt PlayerTurnEvent
  | ResolutionEvt ResolutionEvent
  | ResultEvt ResultEvent
  deriving (Eq, Show)

data BettingCommand = PlaceBet PlayerId Bet
  deriving (Eq, Read, Show)

data BettingEvent = BetPlaced PlayerId Bet
  deriving (Eq, Show)

data DealerTurnCommand = DealerPlay
  deriving (Eq, Read, Show)

newtype DealerTurnEvent = DealerPlayed Dealer
  deriving (Eq, Show)

data DealingCommand = DealInitialCards
  deriving (Eq, Read, Show)

data DealingEvent = CardsDealt [(PlayerId, Hand)] Dealer
  deriving (Eq, Show)

data InsuranceCommand
  = TakeInsurance PlayerId Bet
  | RejectInsurance PlayerId
  | ResolveInsurance
  deriving (Eq, Read, Show)

data InsuranceEvent
  = PlayerTookInsurance PlayerId Bet
  | PlayerDeclinedInsurance PlayerId
  | InsuranceResolved (Map.Map PlayerId InsurancePayout)
  deriving (Eq, Show)

data LobbyCommand
  = JoinGame Text
  | LeaveGame PlayerId
  | StartGame
  deriving (Eq, Read, Show)

data LobbyEvent
  = PlayerJoined PlayerId Text
  | PlayerLeft PlayerId
  | GameStarted
  deriving (Eq, Show)

data PlayerTurnCommand
  = Hit PlayerId
  | Stand PlayerId
  | DoubleDown PlayerId
  | Split PlayerId
  | Surrender PlayerId
  deriving (Eq, Read, Show)

data PlayerTurnEvent
  = HitCard PlayerId Card
  | PlayerStood PlayerId
  | PlayerDoubledDown PlayerId Card
  | PlayerSplitHand PlayerId Card Card Card Card
  | PlayerSurrendered PlayerId
  deriving (Eq, Show)

data ResolutionCommand = ResolveRound
  deriving (Eq, Read, Show)

data ResolutionEvent = RoundResolved DealerOutcome (Map.Map PlayerId PlayerSummary)
  deriving (Eq, Show)

data ResultCommand = RestartGame
  deriving (Eq, Read, Show)

data ResultEvent
  = GameRestarted
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

determineOutcome :: PlayerRound -> HandState -> DealerOutcome -> Outcome
determineOutcome PlayerRound {hasSurrendered} HandState {hand} = \case
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
    payout m = floor (fromIntegral current * m :: Float)

data InsurancePayout
  = WonInsurancePayout Chips
  | LostInsuranceBet Chips
  | NoInsurance
  deriving (Eq, Show)

payoutForInsurance :: Dealer -> PlayerRound -> InsurancePayout
payoutForInsurance (Dealer dealerHand) PlayerRound {insurance} = case insurance of
  Just (TookInsurance (Bet amt))
    | isBlackjack dealerHand -> WonInsurancePayout (amt * 2) -- 2:1 insurance payout if the dealer has a blackjack
    | otherwise -> LostInsuranceBet amt
  _ -> NoInsurance

data GameError
  = PlayerAlreadyJoined
  | GameAlreadyStarted
  | PlayerNotFound PlayerId
  | PlayerRoundNotFound
  | TooFewPlayers
  | PlayerAlreadyActed
  | BadCommand
  | MalsizedBet
  | PlayerAlreadyBet PlayerId
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

extractSplitPair :: Hand -> Maybe (Card, Card)
extractSplitPair hand@(Hand [a, b]) | canSplit hand = Just (a, b)
extractSplitPair _ = Nothing

canSplit :: Hand -> Bool
canSplit (Hand [a, b]) = rank a == rank b
canSplit _ = False

hasAce :: Hand -> Bool
hasAce (Hand hand) = any isAce hand

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
