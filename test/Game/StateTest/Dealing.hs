{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Game.StateTest.Dealing (dealCommand) where

import Domain
import GHC.Generics (Generic)
import Game.StateTest.Model
import GameTopology (GamePhase (DealingCards, PlayerTurn))
import Hedgehog
import Hedgehog.Gen qualified as Gen

data DealCmd v = DealCmd
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FunctorB, TraversableB)

dealCmdGen :: (MonadGen m) => Model v -> m (DealCmd v)
dealCmdGen _ = Gen.constant DealCmd

dealCmdToDomain :: DealCmd Concrete -> Domain.Command
dealCmdToDomain _ = DealInitialCards

dealCommand :: (MonadGen gen) => Hedgehog.Command gen TestContext Model
dealCommand = mkCommand gen toDomain matchDecision failReason callbacks
  where
    gen model = if phase model == DealingCards then Just (dealCmdGen model) else Nothing
    toDomain = dealCmdToDomain
    matchDecision = \case Right (CardsDealt ps _) -> Just ps; _ -> Nothing
    failReason = UnexpectedCommand
    callbacks =
      [ Require \model _ -> phase model == DealingCards,
        Update \model _ _output -> model {phase = PlayerTurn},
        Ensure \_ after _ _output -> do
          phase after /== DealingCards
      ]
