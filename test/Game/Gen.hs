{-# LANGUAGE DataKinds #-}

module Game.Gen (module Game.Gen) where

import Control.Monad (replicateM)
import Data.Text (Text)
import Domain
import GameTopology
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Random (mkStdGen)

genRank :: Gen Rank
genRank = Gen.enumBounded

genSuit :: Gen Suit
genSuit = Gen.enumBounded

genCard :: Gen Card
genCard = liftA2 Card genRank genSuit

genHand :: Gen Hand
genHand = Hand <$> Gen.list (Range.linear 1 5) genCard

genTwoCardHand :: Gen Hand
genTwoCardHand = Hand <$> replicateM 2 genCard

genDealer :: Gen Dealer
genDealer = fmap Dealer genHand

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

genLobbyStateGame :: Gen (Game InLobby)
genLobbyStateGame = do
  stdGen <- fmap mkStdGen Gen.enumBounded
  nextPlayerId <- Gen.int (Range.linear 0 1000)
  playerMap <- genPlayerMap
  let game = Game stdGen nextPlayerId (LobbyState playerMap)
  pure game

genBettingStateGame :: Gen (Game AwaitingBets)
genBettingStateGame = do
  stdGen <- fmap mkStdGen Gen.enumBounded
  nextPlayerId <- Gen.int (Range.linear 0 1000)
  playerMap <- Gen.filter (not . null) genPlayerMap
  let game = Game stdGen nextPlayerId (BettingState playerMap)
  pure game

data SomeGame = forall p. SomeGame (Game p)

genGame :: Gen SomeGame
genGame =
  Gen.choice
    [ fmap SomeGame genLobbyStateGame,
      fmap SomeGame genBettingStateGame
    ]