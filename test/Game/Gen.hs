{-# LANGUAGE DataKinds #-}

module Game.Gen (module Game.Gen) where

import Control.Monad (replicateM)
import Data.List.NonEmpty.Zipper qualified as Z
import Data.Map.Strict qualified as Map
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

genStdGen :: Gen StdGen
genStdGen = fmap mkStdGen Gen.enumBounded

genGameContext :: Gen GameContext
genGameContext =
  GameContext
    <$> genDeck
    <*> Gen.map (Range.linear 1 20) (liftA2 (,) genPlayerId genPlayerRound)
    <*> genDealer

genLobbyStateGame :: Gen (Game InLobby)
genLobbyStateGame = do
  stdGen <- genStdGen
  nextPlayerId <- Gen.int (Range.linear 0 1000)
  playerMap <- genPlayerMap
  let game = Game stdGen nextPlayerId (LobbyState playerMap)
  pure game

genBettingStateGame :: Gen (Game AwaitingBets)
genBettingStateGame = do
  stdGen <- genStdGen
  nextPlayerId <- Gen.int (Range.linear 0 1000)
  playerMap <- Gen.filter (not . null) genPlayerMap
  let game = Game stdGen nextPlayerId (BettingState playerMap)
  pure game

genDealingStateGame :: Gen (Game DealingCards)
genDealingStateGame = do
  stdGen <- genStdGen
  nextPlayerId <- Gen.int (Range.linear 0 1000)
  playerMap <- Gen.filter (not . null) genPlayerMap
  deck <- genDeck
  let game = Game stdGen nextPlayerId (DealingState playerMap deck)
  pure game

genOfferingInsuranceStateGame :: Gen (Game OfferingInsurance)
genOfferingInsuranceStateGame = do
  stdGen <- genStdGen
  nextPlayerId <- Gen.int (Range.linear 0 1000)
  gameContext <- genGameContext
  let game = Game stdGen nextPlayerId (OfferingInsuranceState gameContext)
  pure game

genResolvingInsuranceStateGame :: Gen (Game ResolvingInsurance)
genResolvingInsuranceStateGame = do
  stdGen <- genStdGen
  nextPlayerId <- Gen.int (Range.linear 0 1000)
  gameContext <- genGameContext
  let game = Game stdGen nextPlayerId (ResolvingInsuranceState gameContext)
  pure game

genOpeningTurnStateGame :: Gen (Game OpeningTurn)
genOpeningTurnStateGame = do
  stdGen <- genStdGen
  nextPlayerId <- Gen.int (Range.linear 0 1000)
  gameContext@(GameContext _ rounds _) <- genGameContext
  let pids = Map.keysSet rounds
  payouts <- Gen.map (Range.linear 0 10) (liftA2 (,) (Gen.element pids) genInsurancePayout)
  readyPlayers <- Gen.subset pids
  let insuranceContext = InsuranceContext gameContext payouts
  let openingContext = OpeningContext insuranceContext readyPlayers
  let game = Game stdGen nextPlayerId (OpeningTurnState openingContext)
  pure game

genPlayerTurnStateGame :: Gen (Game PlayerTurn)
genPlayerTurnStateGame = do
  stdGen <- genStdGen
  nextPlayerId <- Gen.int (Range.linear 0 1000)
  gameContext@(GameContext _ rounds _) <- genGameContext
  let pids = Map.keysSet rounds
  payouts <- Gen.map (Range.linear 0 10) (liftA2 (,) (Gen.element pids) genInsurancePayout)
  let context = InsuranceContext gameContext payouts
  let game = Game stdGen nextPlayerId (PlayerTurnState context)
  pure game

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
      fmap SomeGame genPlayerTurnStateGame
    ]

-- genSomePlayerTurnState :: Gen SomeGame
-- genSomePlayerTurnState =
--   Gen.choice [fmap SomeGame genOpeningTurnStateGame, fmap SomeGame genPlayerTurnStateGame]