{-# LANGUAGE DataKinds #-}

module Game.Gen (module Game.Gen) where

import Control.Monad (replicateM)
import Data.List.NonEmpty.Zipper qualified as Z
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Domain
import GameTopology
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Random (StdGen, mkStdGen)

genRank :: Gen Rank
genRank = Gen.enumBounded

genSuit :: Gen Suit
genSuit = Gen.enumBounded

genCard :: Gen Card
genCard = liftA2 Card genRank genSuit

genHand :: Gen Hand
genHand = Hand <$> Gen.list (Range.linear 2 6) genCard

genTwoCardHand :: Gen Hand
genTwoCardHand = Hand <$> replicateM 2 genCard

genBlackjackHand :: Gen Hand
genBlackjackHand = do
  tenCard <- Card <$> Gen.element [Ten, Jack, Queen, King] <*> genSuit
  ace <- Card Ace <$> genSuit
  Gen.element [Hand [ace, tenCard], Hand [tenCard, ace]]

genDealer :: Gen Dealer
genDealer =
  Gen.frequency
    [ (50, fmap Dealer genBlackjackHand),
      (50, fmap Dealer genHand)
    ]

genDeck :: Gen Deck
genDeck =
  Deck
    <$> genStdGen
    <*> Gen.int (Range.linear 0 200)
    <*> Gen.int (Range.linear 4 8)

genPlayerName :: Gen Text
genPlayerName = Gen.text (Range.linear 3 8) Gen.alphaNum

genPlayerId :: Gen PlayerId
genPlayerId = PlayerId <$> Gen.int (Range.linear 0 100)

genPlayerStack :: Gen PlayerStack
genPlayerStack = do
  chips <- genChips
  bet <- genBet chips
  pure (PlayerStack bet chips)

genBet :: Chips -> Gen Bet
genBet = fmap Bet . Gen.int . Range.linear 1

genChips :: Gen Chips
genChips = Gen.int (Range.linear 1 10000)

genPlayer :: Gen Player
genPlayer = Player <$> genPlayerId <*> genPlayerStack <*> genPlayerName

genPlayerMap :: Gen PlayerMap
genPlayerMap = Gen.map (Range.linear 0 100) (liftA2 (,) genPlayerId genPlayer)

genPlayerRound :: Gen PlayerRound
genPlayerRound =
  PlayerRound
    <$> genPlayer
    <*> fmap Z.fromNonEmpty (Gen.nonEmpty (Range.linear 1 5) genHandState)
    <*> Gen.maybe genInsuranceChoice
    <*> Gen.bool

genPlayerRounds :: Gen (Map.Map PlayerId PlayerRound)
genPlayerRounds =
  Gen.map (Range.linear 1 20) $
    liftA2 (,) genPlayerId genPlayerRound

genHandState :: Gen HandState
genHandState =
  HandState
    <$> genHand
    <*> genBet 1000
    <*> Gen.bool
    <*> Gen.bool

genInsuranceChoice :: Gen InsuranceChoice
genInsuranceChoice =
  Gen.choice
    [ TookInsurance <$> genBet 1000,
      Gen.constant DeclinedInsurance
    ]

genInsurancePayout :: Gen InsurancePayout
genInsurancePayout =
  Gen.choice
    [ fmap WonInsurancePayout genChips,
      fmap LostInsuranceBet genChips,
      Gen.constant NoInsurance
    ]

genInsurancePayouts :: Set.Set PlayerId -> Gen (Map.Map PlayerId InsurancePayout)
genInsurancePayouts pids =
  Gen.map (Range.linear 0 (length pids)) (liftA2 (,) (Gen.element pids) genInsurancePayout)

genStdGen :: Gen StdGen
genStdGen = fmap mkStdGen Gen.enumBounded

genNextPlayerId :: Gen Int
genNextPlayerId = Gen.int (Range.linear 0 1000)

genGameContext :: Gen GameContext
genGameContext =
  GameContext
    <$> genDeck
    <*> genPlayerRounds
    <*> genDealer

genInsuranceContext :: Gen InsuranceContext
genInsuranceContext = do
  context@(GameContext _ rounds _) <- genGameContext
  payouts <- genInsurancePayouts (Map.keysSet rounds)
  pure (InsuranceContext context payouts)

genOpeningContext :: Gen OpeningContext
genOpeningContext = do
  insuranceContext <- genInsuranceContext
  readyPlayers <- Gen.subset (Map.keysSet (rounds (context insuranceContext)))
  pure (OpeningContext insuranceContext readyPlayers)

genResolutionContext :: Gen ResolutionContext
genResolutionContext = do
  rounds <- genPlayerRounds
  dealer <- genDealer
  payouts <- genInsurancePayouts (Map.keysSet rounds)
  pure (ResolutionContext rounds dealer payouts)

genLobbyStateGame :: Gen (Game InLobby)
genLobbyStateGame =
  Game
    <$> genStdGen
    <*> genNextPlayerId
    <*> fmap LobbyState genPlayerMap

genBettingStateGame :: Gen (Game AwaitingBets)
genBettingStateGame =
  Game
    <$> genStdGen
    <*> genNextPlayerId
    <*> fmap BettingState (Gen.filter (not . null) genPlayerMap)

genDealingStateGame :: Gen (Game DealingCards)
genDealingStateGame =
  Game
    <$> genStdGen
    <*> genNextPlayerId
    <*> liftA2 DealingState (Gen.filter (not . null) genPlayerMap) genDeck

genOfferingInsuranceStateGame :: Gen (Game OfferingInsurance)
genOfferingInsuranceStateGame =
  Game
    <$> genStdGen
    <*> genNextPlayerId
    <*> fmap OfferingInsuranceState genGameContext

genResolvingInsuranceStateGame :: Gen (Game ResolvingInsurance)
genResolvingInsuranceStateGame =
  Game
    <$> genStdGen
    <*> genNextPlayerId
    <*> fmap ResolvingInsuranceState genGameContext

genOpeningTurnStateGame :: Gen (Game OpeningTurn)
genOpeningTurnStateGame =
  Game
    <$> genStdGen
    <*> genNextPlayerId
    <*> fmap OpeningTurnState genOpeningContext

genPlayerTurnStateGame :: Gen (Game PlayerTurn)
genPlayerTurnStateGame =
  Game
    <$> genStdGen
    <*> genNextPlayerId
    <*> fmap PlayerTurnState genInsuranceContext

genDealerTurnStateGame :: Gen (Game DealerTurn)
genDealerTurnStateGame =
  Game
    <$> genStdGen
    <*> genNextPlayerId
    <*> fmap DealerTurnState genInsuranceContext

genResolutionStateGame :: Gen (Game ResolvingHands)
genResolutionStateGame =
  Game
    <$> genStdGen
    <*> genNextPlayerId
    <*> fmap ResolvingState genResolutionContext

genResultStateGame :: Gen (Game Result)
genResultStateGame = do
  Game
    <$> genStdGen
    <*> genNextPlayerId
    <*> fmap ResultState genPlayerMap

data SomeGame = forall p. SomeGame (Game p)

genGame :: Gen SomeGame
genGame =
  Gen.choice
    [ fmap SomeGame genLobbyStateGame,
      fmap SomeGame genBettingStateGame,
      fmap SomeGame genDealingStateGame,
      fmap SomeGame genOfferingInsuranceStateGame,
      fmap SomeGame genResolvingInsuranceStateGame,
      fmap SomeGame genOpeningTurnStateGame,
      fmap SomeGame genPlayerTurnStateGame,
      fmap SomeGame genDealerTurnStateGame,
      fmap SomeGame genResolutionStateGame,
      fmap SomeGame genResultStateGame
    ]
