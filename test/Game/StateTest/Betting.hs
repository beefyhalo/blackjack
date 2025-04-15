{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Game.StateTest.Betting (placeBetCommand) where

import Data.Set qualified as Set
import Domain
import GHC.Generics (Generic)
import Game.StateTest.Model
import GameTopology (GamePhase (AwaitingBets, DealingCards))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

data PlaceBetCmd v = PlaceBetCmd (Var PlayerId v) Bet
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FunctorB, TraversableB)

placeBetCmdGen :: (MonadGen m) => Model v -> m (PlaceBetCmd v)
placeBetCmdGen model =
  PlaceBetCmd
    <$> Gen.element (players model)
    <*> fmap Bet (Gen.int (Range.linear 1 100))

placeBetCmdToDomain :: PlaceBetCmd Concrete -> Domain.Command
placeBetCmdToDomain (PlaceBetCmd pid bet) = PlaceBet (concrete pid) bet

placeBetCommand :: (MonadGen gen) => Hedgehog.Command gen TestContext Model
placeBetCommand = mkCommand gen toDomain matchDecision failReason callbacks
  where
    gen model = if phase model == AwaitingBets && not (null (players model)) then Just (placeBetCmdGen model) else Nothing
    toDomain = placeBetCmdToDomain
    matchDecision = \case
      Right (BetPlaced pid _) -> Just pid
      Left (PlayerAlreadyBet pid) -> Just pid
      Left BadCommand -> Just (PlayerId (-1)) -- dummy
      _ -> Nothing
    failReason = UnexpectedCommand
    callbacks =
      [ Require \model (PlaceBetCmd pid _) -> phase model == AwaitingBets && not (null (players model)) && Set.notMember pid (playersBetted model),
        Update \model _ output -> model {playersBetted = Set.insert output (playersBetted model), phase = if playersBetted model == players model then DealingCards else phase model},
        Ensure \_ after _ pid -> do
          let playerId = Var (Concrete pid)
          assert (Set.member playerId (playersBetted after))
      ]

-- placeDupeBetCmdGen :: (MonadGen m) => Model v -> m (PlaceBetCmd v)
-- placeDupeBetCmdGen model =
--   PlaceBetCmd
--     <$> Gen.element (playersBetted model)
--     <*> fmap Bet (Gen.int (Range.linear 1 100))

-- placeDupeBetCommand :: (MonadGen gen) => Hedgehog.Command gen TestContext Model
-- placeDupeBetCommand = mkCommand gen toDomain matchDecision failReason callbacks
--   where
--     gen model = if phase model == AwaitingBets && not (null (playersBetted model)) then Just (placeDupeBetCmdGen model) else Nothing
--     toDomain = placeBetCmdToDomain
--     matchDecision = \case Left (PlayerAlreadyBet _) -> Just (); _ -> Nothing
--     failReason = UnexpectedCommand
--     callbacks =
--       [ Require \model _ -> phase model == AwaitingBets && not (null (playersBetted model)),
--         Ensure \before after _ _ -> do
--           before === after
--       ]
